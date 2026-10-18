#lang racket/base
;; Reading a package directory's info.rkt: deps, replaces, and what it links.
(require racket/match racket/list racket/set racket/string racket/path racket/promise
         setup/getinfo pkg/lib pkg/name compiler/module-suffix)
(provide pkg-deps pkg-version pkg-replaces pkg-links info-cache-entries pkg-modules pkg-module-files
         module-suffixes std-pkg? std-version)

;; Packages that ship with the Racket installation act like Go's std:
;; they are never resolved by MVS.
(define std (delay (list->set (installed-pkg-names #:scope 'installation))))
(define (std-pkg? name) (set-member? (force std) name))

(define (std-version name)
  (if (equal? name "base")
      (version)
      (let ([d (pkg-directory name)])
        (or (and d ((info d) 'version (λ () #f))) "0.0"))))

(define (info dir)
  (or (and (file-exists? (build-path dir "info.rkt")) (get-info/full dir))
      (λ (key [default (λ () (error 'info "no ~a" key))]) (default))))

;; the version a package directory declares
(define (pkg-version dir) ((info dir) 'version (λ () "0.0")))

;; -> (listof (cons name floor-string-or-#f))
;; build-deps only matter for the package being developed (docs, tests), like
;; Go's pruning of dependencies' test deps.
(define (pkg-deps dir #:build-deps? [build? #t])
  (define i (info dir))
  (remove-duplicates
    (for/list ([d (in-list (append (i 'deps (λ () '())) (if build? (i 'build-deps (λ () '())) '())))])
      (match d
        [(? string? s) (cons (dep-name s) #f)]
        [(cons (? string? s) kws) (cons (dep-name s) (kw-ref kws '#:version))]))))

(define (dep-name s) (or (package-source->name s) s))

(define (kw-ref l kw)
  (match l
    [(list* (== kw) v _) v]
    [(list* _ _ rest) (kw-ref rest kw)]
    [_ #f]))

;; (define mod-replace '(("pkg" "../local/path") ...)) in the root info.rkt
;; -> (listof (cons name absolute-path))
(define (pkg-replaces dir)
  (for/list ([r (in-list ((info dir) 'mod-replace (λ () '())))])
    (match r
      [(list (? string? n) (? string? p))
       (cons n (simplify-path (path->complete-path p dir)))]
      [_ (error 'raco-mod "malformed mod-replace entry: ~s" r)])))

;; -> links.rktd entries for this package
(define (pkg-links name dir)
  (define d (path->string dir))
  (match ((info dir) 'collection (λ () 'use-pkg-name))
    ['multi (list (list 'root d))]
    ['use-pkg-name (list (list name d))]
    [(? string? c) (list (list c d))]))

;; -> info-cache.rktd entries for the collections these links.rktd entries
;;    provide. `raco setup` normally writes this cache; find-relevant-directories
;;    reads it, e.g. raco looks up `raco-commands` there.
;;    Entry: (dir-bytes fields (lib "coll/sub") 1 0)
(define (info-cache-entries links)
  (define (fields dir)
    (with-handlers ([exn:fail? (λ (_) '())])
      (parameterize ([current-namespace (make-base-empty-namespace)])
        ((dynamic-require (build-path dir "info.rkt") '#%info-domain)))))
  (define (subdirs dir [omit '()])
    (for/list ([s (in-list (directory-list dir))]
               #:when (directory-exists? (build-path dir s))
               #:unless (skipped? (build-path dir s))
               #:unless (regexp-match? #rx"^[.]" (path->string s))
               #:unless (member (path->string s) omit))
      (path->string s)))
  (define (walk dir coll)
    (define has-info? (file-exists? (build-path dir "info.rkt")))
    (define omit (if has-info? ((info dir) 'compile-omit-paths (λ () '())) '()))
    (append
      (if has-info?
          (list (list (path->bytes (simplify-path dir)) (fields dir) (list 'lib coll) 1 0))
          '())
      (if (eq? omit 'all)
          '()
          (append* (for/list ([s (in-list (subdirs dir omit))])
                     (walk (build-path dir s) (string-append coll "/" s)))))))
  (append*
    (for/list ([l (in-list links)])
      (match l
        [(list 'root d)
         (append* (for/list ([s (in-list (subdirs d))])
                    (walk (build-path d s) s)))]
        [(list (? string? c) d) (walk (string->path d) c)]))))

;; Module file suffixes: the installation's (Racket's own among them), and those that the
;; given package directories declare in any of their info.rkt files (Rhombus
;; declares #"rhm" in rhombus/info.rkt, not at the package root). Packages in
;; a build list are not installed, so the installation alone does not know them.
;; -> (listof bytes)
(define (module-suffixes dirs)
  (define (declared d)
    (for*/list ([f (in-directory d (λ (p) (not (skipped? p))))]
                #:when (equal? (file-name-from-path f) (string->path "info.rkt"))
                [i (in-value (get-info/full (path-only f)))]
                #:when i
                [field '(module-suffixes doc-module-suffixes)]
                [sfx (in-list (let ([v (i field (λ () '()))]) (if (list? v) v '())))]
                #:when (bytes? sfx))
      sfx))
  (remove-duplicates
    (append (get-module-suffixes)
            (append-map declared dirs))))

(define (skipped? p)
  (member (path->string (file-name-from-path p)) '("compiled" ".git" "doc")))

;; -> module paths like "threading/main.rkt" this package provides,
;;    for detecting two packages that would shadow each other
(define (pkg-modules name dir suffixes)
  (define rx
    (byte-regexp (bytes-append #"[.](" (apply bytes-append (add-between suffixes #"|")) #")$")))
  (define (mods-under base prefix)
    (for/list ([f (in-directory base (λ (p) (not (skipped? p))))]
               #:when (and (file-exists? f)
                           (regexp-match? rx (path->bytes f))
                           ;; collections may be shared; their info.rkt is not a module clash
                           (not (equal? (path->string (file-name-from-path f)) "info.rkt"))))
      (string-append prefix (string-join (map path->string (explode-path (find-relative-path base f))) "/"))))
  (append*
    (for/list ([l (in-list (pkg-links name dir))])
      (match l
        [(list 'root d)
         (append*
           (for/list ([sub (in-list (directory-list d))]
                      #:when (and (directory-exists? (build-path d sub))
                                  (not (regexp-match? #rx"^[.]|^compiled$" (path->string sub)))))
             (mods-under (build-path d sub) (string-append (path->string sub) "/"))))]
        [(list c d) (mods-under d (string-append c "/"))]))))

;; The absolute paths of the module files to compile: those pkg-modules names,
;; minus what each directory's info.rkt lists in compile-omit-paths, as
;; `raco setup` does.
(define (pkg-module-files name dir suffixes)
  (define rx
    (byte-regexp (bytes-append #"[.](" (apply bytes-append (add-between suffixes #"|")) #")$")))
  (define (files-under d [inherited '()])
    (define i (get-info/full d))
    (define omit (if i (i 'compile-omit-paths (λ () '())) '()))
    (define omitted
      (append inherited
              (if (list? omit)
                  (for/list ([o omit] #:when (string? o)) (simplify-path (build-path d o) #f))
                  '())))
    (if (eq? omit 'all)
        '()
        (for*/list ([f (in-list (directory-list d #:build? #t))]
                    #:unless (or (skipped? f) (member (simplify-path f #f) omitted))
                    [m (in-list (cond
                                  [(directory-exists? f) (files-under f omitted)]
                                  [(and (regexp-match? rx (path->bytes f))
                                        (not (equal? (path->string (file-name-from-path f)) "info.rkt")))
                                   (list f)]
                                  [else '()]))])
          m)))
  (append*
    (for/list ([l (in-list (pkg-links name dir))])
      (match l
        [(list 'root d)
         (append*
           (for/list ([sub (in-list (directory-list d))]
                      #:when (and (directory-exists? (build-path d sub))
                                  (not (regexp-match? #rx"^[.]|^compiled$" (path->string sub)))))
             (files-under (build-path d sub))))]
        [(list c d) (files-under d)]))))

(module+ test
  (require rackunit racket/file)

  (define (pkg info-text . files)
    (define d (make-temporary-directory))
    (display-to-file info-text (build-path d "info.rkt"))
    (for ([f files])
      (make-parent-directory* (build-path d f))
      (display-to-file "#lang racket/base\n" (build-path d f)))
    d)

  (test-case "deps and build-deps"
    (define d (pkg "#lang info\n(define deps '(\"base\" (\"a\" #:version \"1.2\") (\"b\" #:platform macosx #:version \"0.3\")))\n(define build-deps '(\"t\" \"a\"))\n"))
    (check-equal? (pkg-deps d) '(("base" . #f) ("a" . "1.2") ("b" . "0.3") ("t" . #f) ("a" . #f)))
    (check-equal? (pkg-deps d #:build-deps? #f) '(("base" . #f) ("a" . "1.2") ("b" . "0.3")))
    (delete-directory/files d))

  (test-case "links follow raco pkg's collection rules"
    (define single (pkg "#lang info\n(define collection \"c\")\n"))
    (define multi (pkg "#lang info\n(define collection 'multi)\n"))
    (define dflt (pkg "#lang info\n"))
    (check-equal? (pkg-links "p" single) (list (list "c" (path->string single))))
    (check-equal? (pkg-links "p" multi) (list (list 'root (path->string multi))))
    (check-equal? (pkg-links "p" dflt) (list (list "p" (path->string dflt))))
    (for-each delete-directory/files (list single multi dflt)))

  (define sfx '(#"rkt" #"ss" #"scrbl"))

  (test-case "module suffixes include those declared in collection info.rkt"
    (define d (make-temporary-directory))
    (display-to-file "#lang info\n(define collection 'multi)\n" (build-path d "info.rkt"))
    (make-directory* (build-path d "lang"))
    (display-to-file "#lang info\n(define module-suffixes '(#\"xyz\"))\n" (build-path d "lang" "info.rkt"))
    (display-to-file "x" (build-path d "lang" "main.xyz"))
    (define all (module-suffixes (list d)))
    (check-not-false (member #"xyz" all))
    (check-not-false (member #"rkt" all))
    (check-equal? (pkg-modules "p" d all) '("lang/main.xyz"))
    (delete-directory/files d))

  (test-case "module files to compile honor compile-omit-paths at any level"
    (define d (pkg "#lang info\n(define collection \"c\")\n(define compile-omit-paths '(\"content\" \"x/skip.rkt\"))\n"
                   "main.rkt" "content/post.scrbl" "x/keep.rkt" "x/skip.rkt" "y/z.rkt"))
    (display-to-file "#lang info\n(define compile-omit-paths 'all)\n" (build-path d "y" "info.rkt"))
    (check-equal? (sort (map (λ (f) (path->string (find-relative-path d f))) (pkg-module-files "p" d sfx))
                        string<?)
                  '("main.rkt" "x/keep.rkt"))
    (delete-directory/files d))

  (test-case "modules, ignoring info.rkt and compiled/"
    (define single (pkg "#lang info\n(define collection \"c\")\n" "main.rkt" "private/x.rkt" "compiled/y.rkt"))
    (define multi (pkg "#lang info\n(define collection 'multi)\n" "data/a.rkt" "data/info.rkt" "README"))
    (check-equal? (sort (pkg-modules "p" single sfx) string<?) '("c/main.rkt" "c/private/x.rkt"))
    (check-equal? (pkg-modules "p" multi sfx) '("data/a.rkt"))
    (for-each delete-directory/files (list single multi)))

  (test-case "mod-replace resolves relative to the package"
    (define d (pkg "#lang info\n(define mod-replace '((\"x\" \"../local-x\")))\n"))
    (check-equal? (pkg-replaces d)
                  (list (cons "x" (simplify-path (build-path d 'up "local-x")))))
    (delete-directory/files d))

  (test-case "installation packages are std"
    (check-true (std-pkg? "base"))
    (check-false (std-pkg? "surely-not-a-real-package"))
    (check-equal? (std-version "base") (version))))
