#lang racket/base
;; Project operations. The only input is the root info.rkt:
;;   deps / build-deps with #:version   MVS requirements (minimums)
;;   mod-replace                        local overrides
;; info.sum is generated output: the id, time and content hash of each version.
(require racket/list racket/match racket/file racket/string racket/set
         version/utils file/sha1
         "mvs.rkt" "fetch.rkt" "pkginfo.rkt" "infoedit.rkt" "atomic.rkt")
(provide find-project-root get! sync! clean! project-env drracket-env! load-build path->arrow
         (struct-out build))

(define (sum-path root) (build-path root "info.sum"))
(define (state-dir root) (build-path root ".rkt"))
(define (dir-name d) (let-values ([(_b n _d?) (split-path d)]) (path->string n)))

(define (log fmt . args) (eprintf "raco mod: ~a\n" (apply format fmt args)))

;; The nearest directory with info.sum, else the nearest with info.rkt.
(define (find-project-root [dir (current-directory)])
  (define (up pred)
    (let loop ([d (simplify-path (path->complete-path dir))])
      (cond [(pred d) d]
            [else (define-values (parent _n _d?) (split-path d))
             (and (path? parent) (loop parent))])))
  (or (up (λ (d) (file-exists? (sum-path d))))
      (up (λ (d) (file-exists? (build-path d "info.rkt"))))
      (error 'raco-mod "no info.rkt found")))

;; ---------------------------------------------------------------- info.sum

;; One line per package version: name ver id time h1.
;; The time is when that content was made; it decides the implicit minimums
;; of the package's own dependencies, so it is pinned along with the content.
;; (cons name ver) -> (pin id time h1)
(struct pin (id time h1) #:prefab)

(define (read-sum root)
  (define f (sum-path root))
  (if (file-exists? f)
      (for/hash ([l (in-list (file->lines f))] #:unless (string=? (string-trim l) ""))
        (match (string-split l)
          [(list n v id t h)
           #:when (string->number t)
           (values (cons n v) (pin id (string->number t) h))]
          [_ (error 'raco-mod "info.sum: malformed line: ~s" l)]))
      (hash)))

(define (write-sum! root sums)
  (define lines
    (for/list ([k (sort (hash-keys sums)
                        (λ (a b) (or (string<? (car a) (car b))
                                     (and (string=? (car a) (car b)) (version<? (cdr a) (cdr b))))))])
      (match-define (pin id t h) (hash-ref sums k))
      (format "~a ~a ~a ~a ~a\n" (car k) (cdr k) id t h)))
  (write-atomically (apply string-append lines) (sum-path root) #:display? #t))

;; ---------------------------------------------------------------- build list

;; dirs : name -> source dir of the selected version
;; sums : (cons name ver) -> pin, every visited non-replaced node
(struct build (root res dirs sums))

(define (load-build root)
  (define old-sums (read-sum root))
  (define sums (make-hash))
  (define replaces (pkg-replaces root))
  (define (replaced name) (assoc name replaces))
  (define self (dir-name root))
  (define (dir-of name v)
    (cond
      [(replaced name) => cdr]
      [else
       (define known (hash-ref old-sums (cons name v) #f))
       (define-values (dir id h1) (ensure-pkg! name v #:commit (and known (pin-id known))))
       (when (and known (not (equal? (pin-h1 known) h1)))
         (error 'raco-mod "checksum mismatch for ~a@~a\n  info.sum: ~a\n  cache:    ~a"
                name v (pin-h1 known) h1))
       (hash-set! sums (cons name v)
                  (pin id (if known (pin-time known) (version-time name v)) h1))
       dir]))
  ;; the version info.sum selected last time: the highest one it records
  (define (pinned-version name)
    (for/fold ([best #f]) ([k (in-hash-keys old-sums)] #:when (equal? (car k) name))
      (if (or (not best) (version<? best (cdr k))) (cdr k) best)))
  ;; at : when the depending content was made (#f for the project itself and
  ;; for local replacements, whose content has no fixed time).
  ;; A dependency's minimum is its #:version, raised to the version that was
  ;; current at `at`: authors rarely write floors, and what they built against
  ;; is what was current. Otherwise an unversioned dep keeps the version
  ;; info.sum pinned, or takes the latest known one (`sync` warns about these).
  ;; A replaced package has whatever version its local directory declares; it
  ;; need not be in any catalog.
  (define (edges-of dir from #:build-deps? [build? #f] #:at [at #f])
    (for*/list ([d (in-list (pkg-deps dir #:build-deps? build?))]
                #:unless (equal? (car d) self)
                #:unless (and (std-pkg? (car d)) (check-std! d from)))
      (match-define (cons n explicit) d)
      (cons n
            (cond
              [(replaced n) => (λ (r) (pkg-version (cdr r)))]
              [at (define implicit (version-at n at))
               (resolve-floor n (if (and explicit (version<? implicit explicit)) explicit implicit))]
              [explicit (resolve-floor n explicit)]
              [else (or (pinned-version n) (latest-known n))]))))
  (define (reqs-of node)
    (match node
      ['root (edges-of root "info.rkt" #:build-deps? #t)]
      [(cons name v)
       (define dir (dir-of name v))
       (edges-of dir (format "~a@~a" name v)
                 #:at (and (not (replaced name)) (pin-time (hash-ref sums (cons name v)))))]))
  (define res (build-list reqs-of version<?))
  (define dirs
    (for/hash ([(n v) (in-hash (result-selected res))]) (values n (dir-of n v))))
  (build root res dirs sums))

;; Installation packages are not selected by MVS, but their floors must hold.
;; Returns #t so the edge is dropped.
(define (check-std! d from)
  (match-define (cons name floor) d)
  (define have (std-version name))
  (when (and floor (version<? have floor))
    (error 'raco-mod "~a requires ~a >= ~a, but the installation has ~a" from name floor have))
  #t)

(define (check-conflicts! b)
  (define owners (make-hash))
  (define suffixes (module-suffixes (hash-values (build-dirs b))))
  (for ([(n d) (in-hash (build-dirs b))])
    (for ([m (pkg-modules n d suffixes)]) (hash-update! owners m (λ (l) (cons n l)) '())))
  (define clashes (for/list ([(m ns) (in-hash owners)] #:when (> (length ns) 1)) (cons m ns)))
  (unless (null? clashes)
    (error 'raco-mod "module conflicts:\n~a"
           (string-join (for/list ([c (take clashes (min 10 (length clashes)))])
                          (format "  ~a provided by ~a" (car c) (string-join (cdr c) ", ")))
                        "\n"))))

(define (path->arrow p)
  (string-join (for/list ([n p]) (if (eq? n 'root) "info.rkt" (format "~a@~a" (car n) (cdr n))))
               " -> "))

;; ---------------------------------------------------------------- commands

;; Compute the build list, record info.sum, and materialize .rkt/ links.
(define (sync! root)
  (define b (load-build root))
  (for ([d (pkg-deps root)] #:unless (or (cdr d) (std-pkg? (car d))))
    (define v (hash-ref (result-selected (build-res b)) (car d) #f))
    (when v
      (log "warning: ~a has no #:version in info.rkt, so it takes the latest known (~a); `raco mod get ~a@latest` to record it"
           (car d) v (car d))))
  (check-conflicts! b)
  (write-sum! root (build-sums b))
  (define self-links
    (pkg-links (dir-name root) root))
  (define links
    (append self-links
            (append* (for/list ([n (sort (hash-keys (build-dirs b)) string<?)])
                       (pkg-links n (hash-ref (build-dirs b) n))))))
  (define links-file (build-path (state-dir root) "addon" (version) "links.rktd"))
  (make-parent-directory* links-file)
  (make-directory* (build-path (state-dir root) "compiled"))
  (with-output-to-file links-file #:exists 'truncate/replace
    (λ () (write links) (newline)))
  ;; what `raco setup` would leave in (find-user-share-dir): without it,
  ;; `raco mod exec raco <cmd>` cannot see the packages' raco-commands
  (define info-cache (build-path (state-dir root) "addon" (version) "share" "info-cache.rktd"))
  (make-parent-directory* info-cache)
  (with-output-to-file info-cache #:exists 'truncate/replace
    (λ () (write (info-cache-entries links)) (newline)))
  (define gi (build-path (state-dir root) ".gitignore"))
  (unless (file-exists? gi) (display-to-file "*\n" gi))
  (define shared (share-compiled! root b))
  (log "~a packages (~a sharing compiled code)" (hash-count (build-dirs b)) shared)
  b)

;; ---------------------------------------------------------------- shared .zo

;; Compiled code is shared between projects through the cache:
;;   <project>/.rkt/compiled/<rerooted package dir>  ->  <cache>/zo/<key>
;; where key covers the package's exact content, the keys of everything it
;; depends on, and the Racket installation. Projects that select the same
;; versions therefore compile each package once. CM still validates every
;; .zo by SHA1, so a shared directory can never make a build incorrect.
;; Not shared: mod-replace'd packages (mutable), anything depending on them,
;; and dependency cycles.
(define (installation-id)
  (define dep (build-path (collection-path "racket") "compiled" "base_rkt.dep"))
  (list (version) (system-type 'vm) (system-type 'target-machine)
        (path->string (find-system-path 'collects-dir))
        (and (file-exists? dep) (call-with-input-file dep sha1))))

(define (compiled-keys root b)
  (define res (build-res b))
  (define sel (result-selected res))
  (define replaced (map car (pkg-replaces root)))
  (define inst (installation-id))
  (define keys (make-hash))
  (define (key-of n)
    (match (hash-ref keys n 'none)
      ['pending #f] ; a cycle
      ['none
       (hash-set! keys n 'pending)
       (define v (hash-ref sel n))
       (define sum (hash-ref (build-sums b) (cons n v) #f))
       (define deps
         (sort (remove-duplicates
                 (for/list ([e (hash-ref (result-graph res) (cons n v) '())]) (car e)))
               string<?))
       (define dep-keys (for/list ([d deps]) (cons d (key-of d))))
       (define k (and sum (not (member n replaced)) (andmap cdr dep-keys)
                      (sha1 (open-input-string (format "~s" (list inst n v sum dep-keys))))))
       (hash-set! keys n k)
       k]
      [k k]))
  (for/hash ([n (in-hash-keys sel)]) (values n (key-of n))))

;; -> number of packages sharing
(define (share-compiled! root b)
  (register-project! root)
  (define croot (build-path (state-dir root) "compiled"))
  (define keys (compiled-keys root b))
  (define links
    (for/hash ([(n k) (in-hash keys)] #:when k)
      (values (reroot-path (hash-ref (build-dirs b) n) croot) #t)))
  ;; links for packages no longer in the build list
  (when (directory-exists? croot)
    (for ([p (in-directory croot (λ (p) (not (link-exists? p))))]
          #:when (and (link-exists? p) (not (hash-ref links p #f))))
      (delete-file p)))
  (for/sum ([(n k) (in-hash keys)])
    (define link (reroot-path (hash-ref (build-dirs b) n) croot))
    (define (clear!)
      (cond [(link-exists? link) (delete-file link)]
            [(directory-exists? link) (delete-directory/files link)]))
    (cond
      [k
       (define target (build-path (cache-root) "zo" k))
       (make-directory* target)
       (unless (and (link-exists? link) (equal? (resolve-path link) target))
         (clear!)
         (make-parent-directory* link)
         (make-file-or-directory-link target link))
       1]
      [else
       ;; compile locally; drop a link left from an earlier build list
       (when (link-exists? link) (delete-file link))
       0])))

;; specs : (listof "name" | "name@ver" | "name@latest" | "name@none")
;; Edits #:version in info.rkt deps, then syncs. If anything fails, info.rkt
;; and info.sum are left as they were.
(define (get! root specs)
  (define info-file (build-path root "info.rkt"))
  (define saved-info (file->string info-file))
  (define saved-sum (and (file-exists? (sum-path root)) (file->string (sum-path root))))
  (with-handlers ([(λ (_) #t)
                   (λ (e)
                     (write-atomically saved-info info-file #:display? #t)
                     (cond [saved-sum (write-atomically saved-sum (sum-path root) #:display? #t)]
                           [(file-exists? (sum-path root)) (delete-file (sum-path root))])
                     (raise e))])
    (get/unsafe! root specs)))

(define (get/unsafe! root specs)
  (define wanted
    (for/list ([s specs])
      (match (string-split s "@")
        [(list n) (cons n "latest")]
        [(list n q) (cons n q)]
        [_ (error 'raco-mod "bad argument ~s (want pkg or pkg@version)" s)])))
  (define resolved
    (for/list ([w wanted])
      (match w
        [(cons n "none") (cons n #f)]
        [(cons n "latest") (cons n (latest-version n))]
        [(cons n q)
         (unless (valid-version? q) (error 'raco-mod "~a: invalid version ~s" n q))
         (define v (resolve-floor n q))
         (unless (equal? v q) (log "~a has no version ~a; using ~a" n q v))
         (cons n v)])))
  (update-deps!
    (build-path root "info.rkt")
    (λ (deps)
      (for/fold ([deps deps]) ([r resolved])
        (match-define (cons name v) r)
        (define (named? d) (equal? (if (string? d) d (car d)) name))
        (cond
          [(not v) (filter (λ (d) (not (named? d))) deps)]
          [(ormap named? deps) (for/list ([d deps]) (if (named? d) (set-dep-version d name v) d))]
          [else (append deps (list (list name '#:version v)))]))))
  (for ([r resolved])
    (match r
      [(cons name #f) (log "removed ~a" name)]
      [(cons name v) (log "~a #:version ~a" name v)]))
  ;; re-pin what was asked for: its current content may have moved on
  ;; (the latest commit of a version, or a republished archive)
  (define named (map car resolved))
  (write-sum! root (for/hash ([(k v) (in-hash (read-sum root))] #:unless (member (car k) named))
                     (values k v)))
  (define b (sync! root))
  (define res (build-res b))
  (for ([r resolved] #:when (cdr r))
    (match-define (cons name v) r)
    (define sel (hash-ref (result-selected res) name #f))
    (when (and sel (not (equal? sel v)))
      (log "note: asked for ~a ~a but MVS selected ~a: ~a" name v sel (path->arrow (why res name)))))
  b)

;; ---------------------------------------------------------------- clean

;; The cache cannot see who uses a zo/<key> directory, so every synced
;; project registers itself; `clean` keeps what registered projects link to.
(define (projects-file) (build-path (cache-root) "projects.rktd"))

(define (registered-projects)
  (define f (projects-file))
  (if (file-exists? f) (map string->path (file->value f)) '()))

(define (write-projects! ps)
  (write-atomically (sort (remove-duplicates (map path->string ps)) string<?) (projects-file)))

(define (register-project! root)
  (define ps (registered-projects))
  (unless (member root ps) (write-projects! (cons root ps))))

;; Remove everything no registered project uses: shared compiled code they
;; do not link to, and packages, downloads and mirrors their info.sum does
;; not name. Projects that are gone are forgotten. dry-run? only reports.
;; -> (values removed-paths bytes live-projects)
(define (clean! #:dry-run? [dry-run? #f])
  (define zo (simplify-path (build-path (cache-root) "zo")))
  (define live (filter (λ (p) (directory-exists? (state-dir p))) (registered-projects)))
  (define used
    (for*/set ([p (in-list live)]
               [c (in-value (build-path (state-dir p) "compiled"))]
               #:when (directory-exists? c)
               [l (in-directory c (λ (l) (not (link-exists? l))))]
               #:when (link-exists? l))
      (simplify-path (resolve-path l))))
  (define zo-garbage
    (if (directory-exists? zo)
        (for/list ([d (directory-list zo #:build? #t)] #:unless (set-member? used (simplify-path d))) d)
        '()))
  (define live-pkgs
    (for*/list ([p (in-list live)] [(k v) (in-hash (read-sum p))])
      (list (car k) (cdr k) (pin-id v))))
  (define bytes (for/sum ([d zo-garbage]) (path-size d)))
  (define cache-garbage (gc-cache! live-pkgs #:dry-run? #t))
  (define all-bytes (+ bytes (for/sum ([d cache-garbage]) (path-size d))))
  (unless dry-run?
    (for-each delete-directory/files zo-garbage)
    (gc-cache! live-pkgs)
    (write-projects! live))
  (values (append zo-garbage cache-garbage) all-bytes live))

(define (project-env root)
  (define st (state-dir root))
  (list (cons "PLTADDONDIR" (path->string (build-path st "addon")))
        ;; the empty trailing element is the symbol 'same; a literal "same" would
        ;; be a relative directory named "same"
        (cons "PLTCOMPILEDROOTS" (string-append (path->string (build-path st "compiled")) ":"))))

;; DrRacket's tools are usually user-scope packages, which project-env hides.
;; For DrRacket, a second addon dir links the build list first and then the
;; globally installed user-scope packages, minus those the build list already
;; provides. `raco setup` found their tools; its info cache is copied over so
;; DrRacket finds them too.
(define (drracket-env! root b #:user-addon [user-addon (find-system-path 'addon-dir)])
  (define st (state-dir root))
  (define proj (build-path st "addon" (version)))
  (define user (build-path user-addon (version)))
  (define dr (build-path st "drracket" (version)))
  (define (read-or-empty p) (if (file-exists? p) (file->value p) '()))
  ;; links.rktd and info-cache.rktd paths: a string, bytes, or a list of
  ;; elements relative to the file's directory
  (define (resolve p base)
    (simplify-path
      (path->complete-path
        (cond [(list? p) (apply build-path (for/list ([e (in-list p)])
                                             (if (bytes? e) (bytes->path-element e) e)))]
              [(bytes? p) (bytes->path p)]
              [else p])
        base)))
  (define taken (set-add (list->set (hash-keys (build-dirs b))) (dir-name root)))
  (define user-links
    (for*/list ([l (in-list (read-or-empty (build-path user "links.rktd")))]
                [d (in-value (resolve (cadr l) user))]
                #:unless (set-member? taken (dir-name d)))
      (list* (car l) (path->string d) (cddr l))))
  (define (under? p dir)
    (define-values (pe de) (values (explode-path p) (explode-path dir)))
    (and (<= (length de) (length pe)) (equal? (take pe (length de)) de)))
  (define user-share (build-path user "share"))
  (define user-info
    (for*/list ([e (in-list (read-or-empty (build-path user-share "info-cache.rktd")))]
                [d (in-value (match (car e)
                               [(cons 'rel p) (resolve p user-share)]
                               [p (resolve p user-share)]))]
                #:when (for/or ([l (in-list user-links)]) (under? d (string->path (cadr l)))))
      (cons (path->bytes d) (cdr e))))
  (define (write-to p v)
    (make-parent-directory* p)
    (with-output-to-file p #:exists 'truncate/replace (λ () (write v) (newline))))
  (write-to (build-path dr "links.rktd")
            (append (read-or-empty (build-path proj "links.rktd")) user-links))
  (write-to (build-path dr "share" "info-cache.rktd")
            (append (read-or-empty (build-path proj "share" "info-cache.rktd")) user-info))
  (list (cons "PLTADDONDIR" (path->string (build-path st "drracket")))
        (assoc "PLTCOMPILEDROOTS" (project-env root))))

;; End to end, offline: local git repos behind a directory catalog.
(module+ test
  (require rackunit racket/system racket/format setup/dirs file/zip file/sha1
           setup/pack net/uri-codec racket/tcp racket/port)

  (define W (make-temporary-directory))
  (define (repo n) (build-path W "repos" n))
  (define app (build-path W "app"))
  (define (write! p s) (make-parent-directory* p) (display-to-file s p #:exists 'truncate/replace))

  (define (run exe dir args #:env [env '()])
    (define vars (environment-variables-copy (current-environment-variables)))
    (for ([kv env])
      (environment-variables-set! vars (string->bytes/utf-8 (car kv)) (string->bytes/utf-8 (cdr kv))))
    (define out (open-output-string))
    (define ok?
      (parameterize ([current-directory dir] [current-environment-variables vars]
                     [current-output-port out] [current-error-port out])
        (apply system* exe args)))
    (values ok? (get-output-string out)))
  (define (git dir . args)
    (define-values (ok? out) (run (find-executable-path "git") dir args))
    (unless ok? (error 'git "~a" out))
    (string-trim out))
  (define (bin n) (build-path (find-console-bin-dir) n))
  ;; run a Racket tool inside the app's isolated environment
  (define (in-dir dir tool . args)
    (define-values (ok? out) (run (bin tool) dir args #:env (project-env dir)))
    (if ok? (string-trim out) (error tool "~a" out)))
  (define (in-app tool . args) (apply in-dir app tool args))
  (define (link-of dir pkg)
    (define c (build-path dir ".rkt" "compiled"))
    (for/first ([p (in-directory c (λ (p) (not (link-exists? p))))]
                #:when (and (link-exists? p)
                            (regexp-match? (regexp (string-append "/" pkg "@[^/]*$")) (path->string p))))
      (resolve-path p)))

  ;; a minimal HTTP/1.0 file server for dir-url and archive sources
  (define (serve-dir! dir)
    (define l (tcp-listen 0 64 #t "127.0.0.1"))
    (define-values (_a port _b _c) (tcp-addresses l #t))
    (thread
      (λ ()
        (let loop ()
          (define-values (in out) (tcp-accept l))
          (thread
            (λ ()
              (define req (read-line in 'return-linefeed))
              (let skip () (define h (read-line in 'return-linefeed))
                (unless (or (eof-object? h) (string=? h "")) (skip)))
              (define f (build-path dir (string-trim (uri-decode (cadr (regexp-match #rx"^GET ([^ ]*) " req)))
                                                     "/" #:right? #f)))
              (if (file-exists? f)
                  (let ([bs (file->bytes f)])
                    (fprintf out "HTTP/1.0 200 OK\r\nContent-Length: ~a\r\n\r\n" (bytes-length bs))
                    (write-bytes bs out))
                  (fprintf out "HTTP/1.0 404 Not Found\r\nContent-Length: 0\r\n\r\n"))
              (close-output-port out)
              (close-input-port in)))
          (loop))))
    port)
  ;; call a project operation, returning its log output
  (define (logged thunk)
    (define e (open-output-string))
    (parameterize ([current-error-port e]) (thunk))
    (get-output-string e))
  (define (selected) (result-selected (build-res (load-build app))))

  (define (mkpkg! n)
    (make-directory* (repo n))
    (git (repo n) "init" "-q" "-b" "main")
    (write! (build-path W "catalog" "pkg" n)
            (format "~s" (hash 'source (format "file://~a" (path->string (repo n)))))))
  ;; t : the commit time, which decides what an unversioned dep resolves to
  (define (commit! n msg t)
    (git (repo n) "add" "-A")
    (define date (format "@~a +0000" t))
    (define-values (ok? out)
      (run (find-executable-path "git") (repo n)
           (list "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "-m" msg)
           #:env (list (cons "GIT_AUTHOR_DATE" date) (cons "GIT_COMMITTER_DATE" date))))
    (unless ok? (error 'git "~a" out)))
  (define (info! n coll ver deps)
    (write! (build-path (repo n) "info.rkt")
            (format "#lang info\n(define collection ~s)\n(define version ~s)\n(define deps '~s)\n"
                    coll ver deps)))
  (define (c-main! dir v)
    (write! (build-path dir "main.rkt")
            (format "#lang racket/base\n(provide c-version)\n(define c-version ~s)\n" v)))

  (dynamic-wind
    (λ ()
      (putenv "RACO_MOD_CACHE" (path->string (build-path W "cache")))
      (putenv "RACO_MOD_CATALOGS" (format "file://~a" (path->string (build-path W "catalog")))))
    (λ ()
      ;; c-lib: 1.0@1000 1.1@2000 1.2@3000 (+ a docs-only commit @3500) 1.3@5000
      (mkpkg! "c-lib")
      (for ([v '("1.0" "1.1" "1.2")] [t '(1000 2000 3000)])
        (info! "c-lib" "c" v '("base")) (c-main! (repo "c-lib") v) (commit! "c-lib" (~a "c " v) t))
      (write! (build-path (repo "c-lib") "README") "readme") (commit! "c-lib" "docs only" 3500)
      (info! "c-lib" "c" "1.3" '("base")) (c-main! (repo "c-lib") "1.3") (commit! "c-lib" "c 1.3" 5000)
      ;; a-lib 1.0 needs c >= 1.0, a-lib 1.1 needs c >= 1.1
      (mkpkg! "a-lib")
      (info! "a-lib" "a" "1.0" '("base" ("c-lib" #:version "1.0")))
      (write! (build-path (repo "a-lib") "main.rkt")
              "#lang racket/base\n(require c)\n(provide a)\n(define a (string-append \"a+\" c-version))\n")
      (commit! "a-lib" "a 1.0" 1500)
      (info! "a-lib" "a" "1.1" '("base" ("c-lib" #:version "1.1"))) (commit! "a-lib" "a 1.1" 2500)
      ;; b-lib 0.5 needs c >= 1.2
      (mkpkg! "b-lib")
      (info! "b-lib" "b" "0.5" '("base" ("c-lib" #:version "1.2")))
      (write! (build-path (repo "b-lib") "main.rkt") "#lang racket/base\n(provide b)\n(define b \"b\")\n")
      (commit! "b-lib" "b 0.5" 3600)
      ;; k-lib@2100 depends on c-lib without a version; l-lib@3200 with a stale floor
      (mkpkg! "k-lib")
      (info! "k-lib" "k" "1.0" '("base" "c-lib"))
      (commit! "k-lib" "k" 2100)
      (mkpkg! "l-lib")
      (info! "l-lib" "l" "1.0" '("base" ("c-lib" #:version "1.0")))
      (commit! "l-lib" "l" 3200)
      ;; the app: only info.rkt
      (write! (build-path app "info.rkt")
              (string-append "#lang info\n(define collection \"app\")\n;; dependencies\n"
                             "(define deps '(\"base\"\n               (\"a-lib\" #:version \"1.0\")\n"
                             "               \"b-lib\"))\n(define pkg-desc \"test app\")\n"))
      (write! (build-path app "main.rkt")
              "#lang racket/base\n(require a b c)\n(printf \"~a ~a c=~a\" a b c-version)\n")

      (test-case "MVS: max of minimums (a needs c 1.0, b needs c 1.2)"
        (check-regexp-match #rx"b-lib has no #:version" (logged (λ () (sync! app))))
        (check-equal? (selected) (make-hash '(("a-lib" . "1.0") ("b-lib" . "0.5") ("c-lib" . "1.2"))))
        (check-equal? (in-app "racket" "main.rkt") "a+1.2 b c=1.2")
        (check-equal? (why (build-res (load-build app)) "c-lib")
                      '(root ("b-lib" . "0.5") ("c-lib" . "1.2"))))

      (test-case "a dependency's minimum is at least what was current when it was committed"
        (logged (λ () (get! app '("k-lib" "l-lib"))))
        (define g (result-graph (build-res (load-build app))))
        (check-equal? (hash-ref g '("k-lib" . "1.0")) '(("c-lib" . "1.1")))
        (check-equal? (hash-ref g '("l-lib" . "1.0")) '(("c-lib" . "1.2")))
        (check-equal? (hash-ref (selected) "c-lib") "1.2")
        (logged (λ () (get! app '("k-lib@none" "l-lib@none")))))

      (test-case "version = last commit before the next bump, pinned in info.sum"
        (define docs (git (repo "c-lib") "log" "--format=%H" "--grep=^docs only$"))
        (check-regexp-match (regexp (~a "^c-lib 1\\.2 " docs " 3500 h1:"))
                            (car (filter (λ (l) (string-prefix? l "c-lib 1.2"))
                                         (file->lines (build-path app "info.sum"))))))

      (test-case "compiled code goes to .rkt/compiled, the cache stays read-only"
        (in-app "raco" "make" "main.rkt")
        (check-equal? (for/list ([p (in-directory (build-path W "cache" "pkg"))]
                                 #:when (regexp-match? #rx"/compiled$" (path->string p)))
                        p)
                      '())
        (check-true (for/or ([p (in-directory (build-path app ".rkt" "compiled"))])
                      (regexp-match? #rx"c-lib@1[.]2[+][0-9a-f]+/compiled/main_rkt[.]zo$" (path->string p))))
        ;; the installation's own .zo files are reused, not recompiled per project
        (check-false (for/or ([p (in-directory (build-path app ".rkt" "compiled"))])
                       (regexp-match? #rx"/collects/racket/" (path->string p)))))

      (test-case "projects with the same build list share compiled code"
        (define app2 (build-path W "app2"))
        (make-directory* app2)
        (copy-file (build-path app "info.rkt") (build-path app2 "info.rkt"))
        (copy-file (build-path app "main.rkt") (build-path app2 "main.rkt"))
        (logged (λ () (sync! app2)))
        (for ([p '("a-lib" "b-lib" "c-lib")])
          (check-equal? (link-of app2 p) (link-of app p) p))
        (check-regexp-match #rx"/zo/[0-9a-f]+$" (path->string (link-of app2 "c-lib")))
        ;; everything was compiled by the first project already
        (check-false (regexp-match? #rx"-lib@" (in-dir app2 "raco" "make" "-v" "main.rkt")))
        (check-equal? (in-dir app2 "racket" "main.rkt") "a+1.2 b c=1.2")
        ;; a different c-lib changes c's key and the keys of everything above it
        (logged (λ () (get! app2 '("c-lib@1.3"))))
        (check-not-equal? (link-of app2 "c-lib") (link-of app "c-lib"))
        (check-not-equal? (link-of app2 "a-lib") (link-of app "a-lib"))
        (check-equal? (in-dir app2 "racket" "main.rkt") "a+1.3 b c=1.3"))

      (test-case "clean removes compiled code no project links to"
        (define junk (build-path W "cache" "zo" "0000junk"))
        (write! (build-path junk "compiled" "x_rkt.zo") "junk")
        ;; a project that was deleted, with code only it used
        (define gone (build-path W "app3"))
        (make-directory* gone)
        (write! (build-path gone "info.rkt") "#lang info\n(define deps '(\"base\" \"k-lib\"))\n")
        (logged (λ () (sync! gone)))
        (define k-zo (link-of gone "k-lib"))
        (check-true (directory-exists? k-zo))
        (delete-directory/files gone)
        (define-values (dry _b1 _l1) (clean! #:dry-run? #t))
        (check-true (directory-exists? junk))
        (define-values (removed bytes live) (clean!))
        ;; also what earlier tests dropped from app's build list (k-lib, l-lib)
        (check-not-false (member junk removed))
        (check-not-false (member k-zo removed))
        (check-equal? dry removed)
        (check-true (positive? bytes))
        (check-false (member gone live))
        (check-false (directory-exists? junk))
        ;; k-lib's package, its mirror, and e-lib's archive versions are unused too
        (define (cached sub rx)
          (for/list ([e (directory-list (build-path W "cache" sub))]
                     #:when (regexp-match? rx (path->string e)))
            e))
        (check-equal? (cached "pkg" #rx"^k-lib@") '())
        (check-not-equal? (cached "pkg" #rx"^c-lib@1[.]2[+]") '())
        (check-equal? (length (cached "vcs" #rx"")) 3) ; a-lib b-lib c-lib
        ;; everything the remaining projects use survives, and still runs
        (for* ([p (list app (build-path W "app2"))] [pkg '("a-lib" "b-lib" "c-lib")])
          (check-true (directory-exists? (link-of p pkg)) (format "~a ~a" p pkg)))
        (check-equal? (in-dir (build-path W "app2") "racket" "main.rkt") "a+1.3 b c=1.3"))

      (test-case "user-scope packages are hidden"
        (define-values (global? _o) (run (bin "racket") app '("-e" "(require curly-fn)")))
        (when global?
          (check-exn exn:fail? (λ () (in-app "racket" "-e" "(require curly-fn)")))))

      (test-case "drracket: build list first, then user-scope packages and their info cache"
        (define b (sync! app))
        (define user (build-path W "user-addon" (version)))
        (write! (build-path user "pkgs" "t-tool" "info.rkt")
                "#lang info\n(define collection \"t-tool\")\n(define t-drracket-tools '())\n")
        (write! (build-path user "pkgs" "t-tool" "main.rkt") "#lang racket/base\n(display \"tool\")\n")
        (write! (build-path user "pkgs" "c-lib" "info.rkt") "#lang info\n(define collection \"c\")\n")
        (c-main! (build-path user "pkgs" "c-lib") "global")
        (write! (build-path user "links.rktd")
                (~s '(("t-tool" (#"pkgs" #"t-tool")) ("c" (#"pkgs" #"c-lib")))))
        (write! (build-path user "share" "info-cache.rktd")
                (~s '(((rel up #"pkgs" #"t-tool") (collection t-drracket-tools) (lib "t-tool") 1 0)
                      ((rel up #"pkgs" #"c-lib") (collection) (lib "c") 1 0))))
        (define env (drracket-env! app b #:user-addon (build-path W "user-addon")))
        (define (dr . args)
          (define-values (ok? out) (run (bin "racket") app args #:env env))
          (if ok? (string-trim out) (error 'racket "~a" out)))
        (check-equal? (dr "-l" "racket/base" "-l" "t-tool") "tool")
        ;; the user's c-lib is skipped: the build list provides c-lib
        (check-equal? (dr "-l" "racket/base" "-l" "c" "-e" "(display c-version)")
                      (in-app "racket" "-l" "racket/base" "-l" "c" "-e" "(display c-version)"))
        (check-equal? (dr "-l" "racket/base" "-l" "setup/getinfo" "-l" "racket/path" "-e"
                          "(for ([d (find-relevant-directories '(t-drracket-tools))]) (display (file-name-from-path d)))")
                      "t-tool"))

      (test-case "get edits #:version in info.rkt in place"
        (logged (λ () (get! app '("a-lib@latest"))))
        (define text (file->string (build-path app "info.rkt")))
        (check-regexp-match #rx"\\(\"a-lib\" #:version \"1.1\"\\)" text)
        (check-regexp-match #rx";; dependencies\n" text)
        (check-regexp-match #rx"\\(define pkg-desc \"test app\"\\)" text)
        (check-equal? (hash-ref (selected) "c-lib") "1.2"))

      (test-case "a downgrade below another floor is overridden, with the reason"
        (check-regexp-match #rx"asked for c-lib 1.0 but MVS selected 1.2: info.rkt -> b-lib@0.5 -> c-lib@1.2"
                            (logged (λ () (get! app '("c-lib@1.0")))))
        (logged (λ () (get! app '("c-lib@1.3"))))
        (check-equal? (hash-ref (selected) "c-lib") "1.3")
        (logged (λ () (get! app '("c-lib@none"))))
        (check-equal? (hash-ref (selected) "c-lib") "1.2")
        (check-exn #rx"no version >= 9.0" (λ () (logged (λ () (get! app '("c-lib@9.0")))))))

      (test-case "a tampered info.sum is rejected"
        (define sum (build-path app "info.sum"))
        (define good (file->string sum))
        (write! sum (regexp-replace #rx"(c-lib 1\\.2 [0-9a-f]+ [0-9]+ )h1:[^\n]*" good "\\1h1:AAAA"))
        (check-exn #rx"checksum mismatch for c-lib@1.2" (λ () (logged (λ () (sync! app)))))
        (write! sum good))

      ;; e-lib is published as a zip whose URL always serves the current build;
      ;; the default source is bogus, the entry for this Racket version is real
      (define (publish-e! ver body)
        (define d (build-path W "e-src"))
        (when (directory-exists? d) (delete-directory/files d))
        (write! (build-path d "info.rkt")
                (format "#lang info\n(define collection \"e\")\n(define version ~s)\n(define deps '(\"base\"))\n" ver))
        (write! (build-path d "main.rkt")
                (format "#lang racket/base\n(provide e-version)\n(define e-version ~s)\n" body))
        (define z (build-path W "dist" "e-lib.zip"))
        (make-parent-directory* z)
        (when (file-exists? z) (delete-file z))
        (parameterize ([current-directory d]) (zip z "info.rkt" "main.rkt"))
        (write! (build-path W "catalog" "pkg" "e-lib")
                (format "~s" (hash 'source (format "file://~a/dist/missing.zip" W)
                                   'checksum "0"
                                   'versions (hash (version)
                                                   (hash 'source (format "file://~a" z)
                                                         'checksum (call-with-input-file z sha1)))))))
      (define (e-version) (in-app "racket" "-l" "racket/base" "-l" "e" "-e" "(display e-version)"))

      (test-case "zip sources: version from the archive's info.rkt, id = sha1"
        (publish-e! "2.0" "e 2.0")
        (logged (λ () (get! app '("e-lib@latest"))))
        (check-equal? (hash-ref (selected) "e-lib") "2.0")
        (check-equal? (e-version) "e 2.0")
        (check-regexp-match #rx"\ne-lib 2[.]0 sha1:[0-9a-f]+ [0-9]+ h1:" (file->string (build-path app "info.sum"))))

      (test-case "zip sources: upgrade, and older versions stay available from the cache"
        (publish-e! "2.1" "e 2.1")
        (logged (λ () (get! app '("e-lib@latest"))))
        (check-equal? (e-version) "e 2.1")
        (logged (λ () (get! app '("e-lib@2.0"))))
        (check-equal? (e-version) "e 2.0"))

      (test-case "zip sources: content that is gone upstream is reported, not guessed"
        (publish-e! "2.1" "e 2.1, rebuilt")
        (define old-cache (getenv "RACO_MOD_CACHE"))
        ;; a fresh machine with the committed info.sum, pinned to e-lib 2.0
        (dynamic-wind
          (λ () (putenv "RACO_MOD_CACHE" (path->string (build-path W "cache2"))))
          (λ () (check-exn #rx"e-lib@2.0: .* now serves different content.*raco mod get e-lib@latest"
                           (λ () (logged (λ () (sync! app))))))
          (λ () (putenv "RACO_MOD_CACHE" old-cache)))
        (logged (λ () (get! app '("e-lib@none")))))

      (define port (serve-dir! (build-path W "www")))
      (define (http p) (format "http://127.0.0.1:~a/~a" port p))
      (define (catalog! n src [checksum #f])
        (write! (build-path W "catalog" "pkg" n)
                (format "~s" (if checksum (hash 'source src 'checksum checksum) (hash 'source src)))))
      (define (publish-f! body)
        (define d (build-path W "www" "f-lib"))
        (write! (build-path d "info.rkt")
                "#lang info\n(define collection \"f\")\n(define version \"3.0\")\n(define deps '(\"base\"))\n")
        (write! (build-path d "main.rkt") (format "#lang racket/base\n(provide f)\n(define f ~s)\n" body))
        (write! (build-path d "MANIFEST") "info.rkt\nmain.rkt\n")
        (catalog! "f-lib" (http "f-lib/") (call-with-input-file (build-path d "main.rkt") sha1)))

      (test-case "remote directory sources (MANIFEST over HTTP)"
        (publish-f! "f one")
        (logged (λ () (get! app '("f-lib"))))
        (check-equal? (hash-ref (selected) "f-lib") "3.0")
        (check-regexp-match #rx"\nf-lib 3[.]0 dir:[0-9a-f]+ [0-9]+ h1:" (file->string (build-path app "info.sum")))
        (check-equal? (in-app "racket" "-l" "racket/base" "-l" "f" "-e" "(display f)") "f one"))

      (test-case "get re-pins a version whose content moved on"
        (publish-f! "f two")
        (logged (λ () (sync! app)))
        (check-equal? (in-app "racket" "-l" "racket/base" "-l" "f" "-e" "(display f)") "f one")
        (logged (λ () (get! app '("f-lib@latest"))))
        (check-equal? (in-app "racket" "-l" "racket/base" "-l" "f" "-e" "(display f)") "f two")
        (logged (λ () (get! app '("f-lib@none")))))

      (test-case ".plt archives, unpacked without evaluating their setup code"
        (define d (build-path W "g-src"))
        (write! (build-path d "info.rkt")
                "#lang info\n(define collection \"g\")\n(define version \"0.4\")\n(define deps '(\"base\"))\n")
        (write! (build-path d "main.rkt") "#lang racket/base\n(provide g)\n(define g \"from plt\")\n")
        (define plt (build-path W "www" "g-lib.plt"))
        (parameterize ([current-directory d] [current-output-port (open-output-nowhere)])
          (pack-plt plt "g-lib" (list "info.rkt" "main.rkt")))
        (catalog! "g-lib" (http "g-lib.plt") (call-with-input-file plt sha1))
        (logged (λ () (get! app '("g-lib"))))
        (check-equal? (hash-ref (selected) "g-lib") "0.4")
        (check-equal? (in-app "racket" "-l" "racket/base" "-l" "g" "-e" "(display g)") "from plt")
        (logged (λ () (get! app '("g-lib@none")))))

      (test-case "HTTP errors are reported"
        (catalog! "h-lib" (http "missing/h-lib.zip"))
        (check-exn #rx"HTTP 404" (λ () (logged (λ () (get! app '("h-lib")))))))

      (test-case "info.sum pins content times, so a refreshed index changes nothing"
        ;; b-lib gets a new commit at 9000 without a version bump; c 1.3 was
        ;; current then, so b-lib's implicit minimum for c would move to 1.3
        (write! (build-path (repo "b-lib") "main.rkt")
                "#lang racket/base\n(provide b)\n(define b \"b2\")\n")
        (commit! "b-lib" "b 0.5, later" 9000)
        (version-index "b-lib" #:refresh? #t)
        (logged (λ () (sync! app)))
        (check-equal? (hash-ref (selected) "c-lib") "1.2")
        (check-regexp-match #rx"\nb-lib 0[.]5 [0-9a-f]+ 3600 h1:" (file->string (build-path app "info.sum")))
        ;; re-pinning b-lib takes the new commit and its time
        (logged (λ () (get! app '("b-lib@latest"))))
        (check-regexp-match #rx"\nb-lib 0[.]5 [0-9a-f]+ 9000 h1:" (file->string (build-path app "info.sum")))
        (check-equal? (hash-ref (selected) "c-lib") "1.3"))

      (test-case "a replaced package need not be in any catalog"
        (define app4 (build-path W "app4"))
        (write! (build-path W "only-local" "info.rkt")
                "#lang info\n(define collection \"z\")\n(define version \"2.5\")\n(define deps '(\"base\"))\n")
        (write! (build-path W "only-local" "main.rkt") "#lang racket/base\n(provide z)\n(define z \"local z\")\n")
        (write! (build-path app4 "info.rkt")
                (string-append "#lang info\n(define deps '(\"base\" (\"z-lib\" #:version \"2.0\")))\n"
                               "(define mod-replace '((\"z-lib\" \"../only-local\")))\n"))
        (logged (λ () (sync! app4)))
        (check-equal? (result-selected (build-res (load-build app4))) (make-hash '(("z-lib" . "2.5"))))
        (check-equal? (in-dir app4 "racket" "-l" "racket/base" "-l" "z" "-e" "(display z)") "local z"))

      (test-case "a failed get leaves info.rkt and info.sum as they were"
        (define app4 (build-path W "app4"))
        (with-output-to-file (build-path app4 "info.rkt") #:exists 'append
          (λ () (display "\n(define build-deps '((\"base\" #:version \"99.0\")))\n")))
        (define info-before (file->string (build-path app4 "info.rkt")))
        (define sum-before (file->string (build-path app4 "info.sum")))
        (check-exn #rx"requires base >= 99.0" (λ () (logged (λ () (get! app4 '("c-lib"))))))
        (check-equal? (file->string (build-path app4 "info.rkt")) info-before)
        (check-equal? (file->string (build-path app4 "info.sum")) sum-before))

      (test-case "mod-replace points at a local checkout"
        (git W "clone" "-q" (path->string (repo "c-lib")) "local-c")
        (c-main! (build-path W "local-c") "local")
        (with-output-to-file (build-path app "info.rkt") #:exists 'append
          (λ () (display "\n(define mod-replace '((\"c-lib\" \"../local-c\")))\n")))
        (logged (λ () (sync! app)))
        (check-equal? (in-app "racket" "main.rkt") "a+local b2 c=local"))

      (test-case "floors on installation packages are checked"
        (with-output-to-file (build-path app "info.rkt") #:exists 'append
          (λ () (display "\n(define build-deps '((\"base\" #:version \"99.0\")))\n")))
        (check-exn #rx"requires base >= 99.0" (λ () (logged (λ () (sync! app)))))))
    (λ ()
      (system* (find-executable-path "chmod") "-R" "u+w" (path->string W))
      (delete-directory/files W))))
