#lang racket/base
;; Module cache: catalog lookup, sources, version index, immutable packages.
;;
;;   <cache>/source/<name>.rktd         catalog entry (source checksum), memoized
;;   <cache>/vcs/<sha1(url)>/           bare git mirror
;;   <cache>/dl/<sha1>.<ext>            downloaded archive, by content
;;   <cache>/index/<name>.rktd          version index: ((ver . id) ...) ascending
;;   <cache>/pkg/<name>@<ver>+<id12>/   extracted package, read-only
;;   <cache>/pkg/<name>@<ver>+<id12>.h1 its content hash
;;
;; A version is the `version` field of the package's info.rkt. Its id says
;; exactly which content that is:
;;   git source                  the commit (see build-index)
;;   archive (.zip .tgz .plt)    "sha1:<hex>" of the archive file
;;   directory (remote or local) "dir:<hex>", a hash of the content
;; Archive and directory sources only ever serve their current content, so
;; older versions of those packages live on only in this cache.
(require racket/file racket/string racket/list racket/match racket/system racket/path
         racket/port racket/set file/sha1 file/unzip file/untgz net/base64 net/url
         setup/unpack pkg/lib pkg/name version/utils "atomic.rkt")
(provide cache-root version-index resolve-floor latest-version latest-known
         version-at version-time ensure-pkg! gc-cache! path-size
         dir-hash parse-git-source info-text-version)

(define (cache-root)
  (cond [(getenv "RACO_MOD_CACHE") => (λ (s) (path->complete-path (string->path s)))]
        [else (build-path (find-system-path 'home-dir) ".cache" "raco-mod")]))

(define (log fmt . args)
  (eprintf "raco mod: ~a\n" (apply format fmt args)))


;; ---------------------------------------------------------------- processes

(define (exe name)
  (or (find-executable-path name) (error 'raco-mod "`~a` not found in PATH" name)))

(define (run! who exe-path args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define ok?
    (parameterize ([current-output-port out] [current-error-port err]
                   [current-input-port (open-input-string "")])
      (apply system* exe-path args)))
  (unless ok?
    (error 'raco-mod "~a ~a failed:\n~a" who (string-join args) (get-output-string err)))
  (string-trim (get-output-string out)))

(define (git dir . args)
  (run! "git" (exe "git") (if dir (list* "-C" (path->string dir) args) args)))

;; ---------------------------------------------------------------- sources

(struct git-src (url subpath branch) #:transparent)
(struct archive-src (url checksum ext) #:transparent)
;; url ends without "/" for a local directory (file:), with MANIFEST otherwise
(struct dir-src (url checksum local?) #:transparent)

;; -> (list source checksum-or-#f); like raco pkg, an entry for the running
;; Racket version in `versions` takes precedence over the default source
(define (catalog-entry name #:refresh? [refresh? #f])
  (define f (build-path (cache-root) "source" (string-append name ".rktd")))
  (cond
    [(and (not refresh?) (file-exists? f)) (file->value f)]
    [else
     (define cats (getenv "RACO_MOD_CATALOGS"))
     (define details
       (parameterize ([current-pkg-catalogs (and cats (map string->url (string-split cats)))])
         (get-pkg-details-from-catalogs name)))
     (unless details (error 'raco-mod "package ~s not found in any catalog" name))
     (define for-us (hash-ref (hash-ref details 'versions (hash)) (version) #f))
     (define (field k) (or (and for-us (hash-ref for-us k #f)) (hash-ref details k #f)))
     (define entry (list (field 'source) (let ([c (field 'checksum)]) (and (non-empty-string? c) c))))
     (write-atomically entry f)
     entry]))

;; Source types are inferred exactly like raco pkg does.
(define (classify src checksum)
  (define-values (_name type) (package-source->name+type src #f))
  (define (as-url s)
    (if (regexp-match? #rx"^[a-z+]+://" s) s (url->string (path->url (path->complete-path s)))))
  (case type
    [(file-url file)
     (define ext (cond [(regexp-match #rx"[.](zip|tgz|tar[.]gz|plt)$" src) => cadr]
                       [else (error 'raco-mod "unsupported archive format: ~s" src)]))
     (archive-src (as-url src) checksum ext)]
    [(dir-url) (dir-src src checksum #f)]
    [(dir)
     (define u (as-url (regexp-replace #rx"^git[+]" src "")))
     ;; a local directory that is a git repository is used as one
     (if (directory-exists? (build-path (url->path (string->url u)) ".git"))
         (call-with-values (λ () (parse-git-source u)) git-src)
         (dir-src u checksum #t))]
    [(git github clone) (call-with-values (λ () (parse-git-source src)) git-src)]
    [else (error 'raco-mod "unsupported package source: ~s (~a)" src type)]))

(define (source-of name #:refresh? [refresh? #f])
  (apply classify (catalog-entry name #:refresh? refresh?)))

;; source -> (values clone-url subpath branch-or-#f)
(define (parse-git-source src)
  (define (clean p) (string-trim (or p "") "/"))
  (match src
    [(regexp #rx"^github://github\\.com/([^/]+)/([^/]+)(?:/([^/]+)(?:/(.*))?)?$" (list _ u r br p))
     (values (format "https://github.com/~a/~a" u r) (clean p) br)]
    [(regexp #rx"^(git\\+)?(git|https?|file)://([^?#]*)(?:\\?path=([^#]*))?(?:#(.*))?$"
             (list _ _ scheme rest p br))
     ;; nobody serves the git:// protocol anymore; raco pkg also maps it to https
     (define real-scheme (if (equal? scheme "git") "https" scheme))
     (values (format "~a://~a" real-scheme rest) (clean p) br)]
    [_ (error 'raco-mod "unsupported package source: ~s" src)]))

;; ---------------------------------------------------------------- info.rkt

;; Find (define version "...") in an info.rkt without evaluating it. Handles
;; `#lang info` and the expanded form found in built packages.
(define (info-text-version text)
  (define in (open-input-string text))
  (when (regexp-match-peek #rx"^#lang" in) (read-line in))
  (define forms (with-handlers ([exn:fail:read? (λ (_) '())])
                  (for/list ([f (in-port read in)]) f)))
  (let find ([forms forms])
    (for/or ([f (in-list forms)])
      (match f
        [(list 'define 'version (? string? v)) v]
        [(list 'module _ _ body ...) (find body)]
        [(list '#%module-begin body ...) (find body)]
        [_ #f]))))

(define (dir-version name dir)
  (define f (build-path dir "info.rkt"))
  (define v (or (and (file-exists? f) (info-text-version (file->string f))) "0.0"))
  (unless (valid-version? v)
    (error 'raco-mod "~a: info.rkt has version ~s, which is not a valid Racket version" name v))
  v)

;; ---------------------------------------------------------------- git

(define (mirror-dir url) (build-path (cache-root) "vcs" (sha1 (open-input-string url))))

(define (ensure-mirror! url #:update? [update? #f])
  (define d (mirror-dir url))
  (cond
    [(directory-exists? d)
     (when update?
       (log "fetching ~a" url)
       (git d "fetch" "--quiet" "--force" "origin" "+refs/heads/*:refs/heads/*"))]
    [else
     (log "cloning ~a" url)
     (make-directory* (build-path (cache-root) "vcs"))
     (define tmp (make-temporary-directory* #"clone-" #"" #:base-dir (build-path (cache-root) "vcs")))
     (with-handlers ([exn:fail? (λ (e) (delete-directory/files tmp #:must-exist? #f) (raise e))])
       (git #f "clone" "--bare" "--quiet" url (path->string tmp)))
     (rename-file-or-directory tmp d)])
  d)

(define (info-path subpath) (if (string=? subpath "") "info.rkt" (string-append subpath "/info.rkt")))

;; Every commit touching the package carries the info.rkt version current at
;; that commit; version V is represented by the *last* such commit. For a
;; version already superseded that commit is fixed forever; for the current
;; one, info.sum pins whichever commit was first seen.
;; Entry times: `since` is when the version first appeared (its first commit),
;; `at` is when the representative content was made (the last commit).
(define (build-index d subpath branch)
  (define ref (or branch "HEAD"))
  (define (commits . pathspec)
    (reverse
      (for/list ([l (string-split (apply git d "log" "--first-parent" "--format=%H %ct" ref "--" pathspec)
                                  "\n")])
        (match (string-split l) [(list c t) (cons c (string->number t))]))))
  (define bumps (for/hash ([c (commits (info-path subpath))]) (values (car c) #t)))
  (define-values (idx _cur)
    (for/fold ([idx '()] [cur #f]) ([ct (in-list (commits (if (string=? subpath "") "." subpath)))])
      (match-define (cons c t) ct)
      (define v
        (if (hash-ref bumps c #f)
            (let ([text (with-handlers ([exn:fail? (λ (_) #f)])
                          (git d "show" (string-append c ":" (info-path subpath))))])
              (and text (or (info-text-version text) "0.0")))
            cur))
      (cond
        [(not (and v (valid-version? v))) (values idx v)]
        [(and (pair? idx) (equal? (e-ver (car idx)) v))
         (values (cons (entry v c (e-since (car idx)) t) (cdr idx)) v)]
        [(or (null? idx) (version<? (e-ver (car idx)) v)) (values (cons (entry v c t t) idx) v)]
        [else (values idx v)])))
  (reverse idx))

;; extract `commit` of a git source into a fresh directory -> package root
(define (git-extract! name s commit tmp)
  (define d (ensure-mirror! (git-src-url s)))
  (unless (with-handlers ([exn:fail? (λ (_) #f)])
            (git d "cat-file" "-e" (string-append commit "^{commit}")) #t)
    (ensure-mirror! (git-src-url s) #:update? #t))
  (define subpath (git-src-subpath s))
  (define tar (build-path tmp "src.tar"))
  (apply git d "archive" "--format=tar" "-o" (path->string tar) commit
         (if (string=? subpath "") '() (list "--" subpath)))
  (define out (build-path tmp "out"))
  (make-directory out)
  (run! "tar" (exe "tar") (list "-xf" (path->string tar) "-C" (path->string out)))
  (define root (if (string=? subpath "") out (build-path out subpath)))
  (unless (directory-exists? root)
    (error 'raco-mod "~a: path ~s missing in repository" name subpath))
  root)

;; ---------------------------------------------------------------- archives

(define (dl-file h ext) (build-path (cache-root) "dl" (string-append h "." ext)))

;; copy a file: or http(s): URL to dest
(define (fetch-url! u dest)
  (define url (if (url? u) u (string->url u)))
  (with-handlers ([exn:fail? (λ (e) (when (file-exists? dest) (delete-file dest)) (raise e))])
    (call-with-output-file dest #:exists 'truncate
      (λ (out)
        (cond
          [(equal? (url-scheme url) "file")
           (call-with-input-file (url->path url) (λ (in) (copy-port in out)))]
          [else
           (define-values (in headers)
             (get-pure-port/headers url #:redirections 10 #:status? #t))
           (define status (string->number (cadr (regexp-match #rx"^[^ ]+ ([0-9]+)" headers))))
           (unless (= status 200)
             (close-input-port in)
             (error 'raco-mod "download ~a failed: HTTP ~a" (url->string url) status))
           (copy-port in out)
           (close-input-port in)])))))

;; -> (values file sha1)
(define (download! s)
  (define dl (build-path (cache-root) "dl"))
  (make-directory* dl)
  (define tmp (make-temporary-file "dl-~a" #f dl))
  (log "downloading ~a" (archive-src-url s))
  (fetch-url! (archive-src-url s) tmp)
  (define h (call-with-input-file tmp sha1))
  (rename-file-or-directory tmp (dl-file h (archive-src-ext s)) #t)
  (values (dl-file h (archive-src-ext s)) h))

;; archives come from the network: refuse entries that escape the destination
(define (safe-entry? name)
  (define p (if (path? name) name (bytes->path name)))
  (and (relative-path? p)
       (not (memq 'up (explode-path (simplify-path p #f))))))

;; extract an archive into a fresh directory -> package root
(define (archive-extract! file ext tmp)
  (define out (build-path tmp "out"))
  (make-directory* out)
  (call-with-input-file file
    (λ (in)
      (case ext
        [("plt")
         ;; the archive's config and setup unit are data here; never evaluated
         (define (dest d)
           (define p (match d ['same (build-path 'same)] [(list _loc p) p] [p p]))
           (unless (safe-entry? p) (error 'raco-mod "unsafe path in archive: ~a" p))
           (build-path out p))
         (close-input-port in)
         (fold-plt-archive file
                           (λ (_cfg acc) acc)
                           (λ (_unit _in acc) acc)
                           (λ (d acc) (make-directory* (dest d)) acc)
                           (λ (f fin acc)
                             (make-parent-directory* (dest f))
                             (call-with-output-file (dest f) #:exists 'truncate
                               (λ (o) (copy-port fin o)))
                             acc)
                           (void))]
        [("zip")
         (define dir (read-zip-directory file))
         (for ([e (zip-directory-entries dir)])
           (unless (safe-entry? e) (error 'raco-mod "unsafe path in archive: ~a" e)))
         (unzip in (make-filesystem-entry-reader #:dest out #:exists 'replace))]
        [else
         (untgz in #:dest out
                #:filter (λ (p _type . _)
                           (unless (safe-entry? p) (error 'raco-mod "unsafe path in archive: ~a" p))
                           #t))])))
  ;; like raco pkg: a lone top-level directory is the package itself
  (define entries (directory-list out))
  (if (and (= 1 (length entries))
           (directory-exists? (build-path out (car entries)))
           (not (file-exists? (build-path out "info.rkt"))))
      (build-path out (car entries))
      out))

;; ---------------------------------------------------------------- directories

;; Fetch the current content of a directory source into tmp -> package root.
;; Remote directories list their files in MANIFEST, as raco pkg expects.
(define (dir-fetch! s tmp)
  (define out (build-path tmp "out"))
  (cond
    [(dir-src-local? s)
     (copy-directory/files (url->path (string->url (dir-src-url s))) out)]
    [else
     (log "downloading ~a" (dir-src-url s))
     (define base (string->url (dir-src-url s)))
     (define (url-like f)
       ;; like raco pkg: the last path element is a directory
       (if (and (pair? (url-path base)) (equal? "" (path/param-path (last (url-path base)))))
           (combine-url/relative base f)
           (struct-copy url base [path (append (url-path base) (list (path/param f null)))])))
     (make-directory* out)
     (define manifest (build-path tmp "MANIFEST"))
     (fetch-url! (url-like "MANIFEST") manifest)
     (for ([f (file->lines manifest)] #:unless (string=? (string-trim f) ""))
       (unless (safe-entry? (string->path f)) (error 'raco-mod "unsafe path in MANIFEST: ~a" f))
       (make-parent-directory* (build-path out f))
       (fetch-url! (url-like f) (build-path out f)))])
  out)

(define (dir-id root) (string-append "dir:" (sha1 (open-input-string (dir-hash root)))))

;; Like archives, a directory serves only its current content. The catalog
;; checksum (when there is one) avoids refetching unchanged content.
(define (dir-index name s cached)
  (define seen-file (build-path (cache-root) "index" (string-append name ".seen.rktd")))
  (define seen (if (file-exists? seen-file) (file->value seen-file) (hash)))
  (define cs (dir-src-checksum s))
  (cond
    [(and cs (not (dir-src-local? s)) (hash-ref seen cs #f) (find-id (hash-ref seen cs) cached))
     cached]
    [else
     (define-values (v id)
       (with-tmp (λ (tmp)
                   (define root (dir-fetch! s tmp))
                   (define id (dir-id root))
                   (values (materialize! name #f id root) id))))
     (when cs (write-atomically (hash-set seen cs id) seen-file))
     (add-entry cached v id)]))

(define (archive-id h) (string-append "sha1:" h))
(define (id->sha1 id) (cadr (regexp-match #rx"^sha1:([0-9a-f]+)$" id)))

;; The archive URL serves one version: the current one. The index accumulates
;; every version seen, which this cache can still provide.
(define (archive-index name s cached)
  (define cs (archive-src-checksum s))
  (cond
    [(and cs (find-id (archive-id cs) cached)) cached]
    [else
     (define-values (file h) (download! s))
     ;; like raco pkg: content that does not match the catalog is refused
     (when (and cs (not (equal? cs h)))
       (delete-file file)
       (error 'raco-mod "~a: ~a does not match the catalog (sha1 ~a, catalog says ~a)"
              name (archive-src-url s) h cs))
     (define v (with-tmp (λ (tmp) (materialize! name #f (archive-id h)
                                                (archive-extract! file (archive-src-ext s) tmp)))))
     (add-entry cached v (archive-id h))]))

;; Archives and directories have no history: their times are when this cache
;; first saw the version and its current content.
(define (add-entry idx v id)
  (define old (find-ver v idx))
  (define now (current-seconds))
  (sort (cons (entry v id (if old (e-since old) now) (if (and old (equal? (e-id old) id)) (e-at old) now))
              (filter (λ (e) (not (equal? (e-ver e) v))) idx))
        version<? #:key e-ver))

;; ---------------------------------------------------------------- index

;; an index entry: (list ver id since at), times in seconds
(define (entry v id since at) (list v id since at))
(define (e-ver e) (car e))
(define (e-id e) (cadr e))
(define (e-since e) (caddr e))
(define (e-at e) (cadddr e))
(define (find-ver v idx) (findf (λ (e) (equal? (e-ver e) v)) idx))
(define (find-id id idx) (findf (λ (e) (equal? (e-id e) id)) idx))

(define index-memo (make-hash))
(define (index-file name) (build-path (cache-root) "index" (string-append name ".rktd")))

;; -> (listof entry), ascending by version
(define (version-index name #:refresh? [refresh? #f])
  (define f (index-file name))
  (define cached
    (let ([v (if (file-exists? f) (file->value f) '())])
      (if (and (list? v) (andmap (λ (e) (and (list? e) (= 4 (length e)))) v)) v '())))
  (cond
    [(and (not refresh?) (hash-ref index-memo name #f))]
    [(and (not refresh?) (pair? cached)) (hash-set! index-memo name cached) cached]
    [else
     (define idx
       (match (source-of name #:refresh? refresh?)
         [(? git-src? s)
          (build-index (ensure-mirror! (git-src-url s) #:update? refresh?)
                       (git-src-subpath s) (git-src-branch s))]
         [(? archive-src? s) (archive-index name s cached)]
         [(? dir-src? s) (dir-index name s cached)]))
     (when (null? idx) (error 'raco-mod "~a: no info.rkt history found" name))
     (write-atomically idx f)
     (hash-set! index-memo name idx)
     idx]))

;; The smallest indexed version >= floor (#f floor = no constraint).
;; Only refreshes when the cached index cannot satisfy it.
(define (resolve-floor name floor)
  (define (pick idx)
    (for/first ([e (in-list idx)] #:when (or (not floor) (version<=? floor (e-ver e)))) (e-ver e)))
  (or (pick (version-index name))
      (pick (version-index name #:refresh? #t))
      (error 'raco-mod "~a: no version >= ~a (latest is ~a)"
             name floor (e-ver (last (version-index name))))))

(define (latest-version name)
  (e-ver (last (version-index name #:refresh? #t))))

;; latest in the cached index, fetching only if there is none
(define (latest-known name)
  (e-ver (last (version-index name))))

;; when the content of `name`@`v` was made
(define (version-time name v)
  (define e (or (find-ver v (version-index name)) (find-ver v (version-index name #:refresh? #t))))
  (and e (e-at e)))

;; The version of `name` that was current at time `secs`: what a dependency
;; written without #:version was developed against. Refreshes the index once
;; if it was fetched before `secs`, since a newer version may be missing.
(define refreshed (make-hash))
(define (version-at name secs)
  (define (pick idx)
    (for/last ([e (in-list idx)] #:when (<= (e-since e) secs)) (e-ver e)))
  (define idx (version-index name))
  (define stale? (and (not (hash-ref refreshed name #f))
                      (file-exists? (index-file name))
                      (< (file-or-directory-modify-seconds (index-file name)) secs)))
  (define idx* (if stale?
                   (begin (hash-set! refreshed name #t) (version-index name #:refresh? #t))
                   idx))
  (or (pick idx*) (e-ver (car idx*))))

;; ---------------------------------------------------------------- packages

(define (short id) (substring (regexp-replace #rx"^[a-z0-9]+:" id "") 0 12))
(define (pkg-dir name v id) (build-path (cache-root) "pkg" (format "~a@~a+~a" name v (short id))))
(define (hash-file name v id) (build-path (cache-root) "pkg" (format "~a@~a+~a.h1" name v (short id))))

(define (with-tmp proc)
  (define pkgs (build-path (cache-root) "pkg"))
  (make-directory* pkgs)
  (define tmp (make-temporary-directory* #"x-" #"" #:base-dir pkgs))
  (dynamic-wind void (λ () (proc tmp)) (λ () (delete-directory/files tmp #:must-exist? #f))))

;; Move an extracted package root into the cache, read-only. With v = #f the
;; version is read from its info.rkt. Losing a race to another process that
;; extracted the same content is fine: its copy is identical. -> version
(define (materialize! name v id root)
  (define ver (or v (dir-version name root)))
  (define dir (pkg-dir name ver id))
  (unless (directory-exists? dir)
    ;; chmod only after the rename: moving a directory needs it to be writable
    (when (with-handlers ([exn:fail:filesystem? (λ (e) (if (directory-exists? dir) #f (raise e)))])
            (rename-file-or-directory root dir)
            #t)
      (run! "chmod" (exe "chmod") (list "-R" "a-w" (path->string dir)))))
  ;; also repairs a process that died between the rename and this write
  (unless (file-exists? (hash-file name ver id))
    (write-atomically (dir-hash dir) (hash-file name ver id) #:display? #t))
  ver)

;; pinned : the id recorded in info.sum, if any
;; -> (values dir id h1)
(define (ensure-pkg! name v #:commit [pinned #f])
  (define id
    (or pinned
        (e-id (or (find-ver v (version-index name))
                  (find-ver v (version-index name #:refresh? #t))
                  (error 'raco-mod "~a: version ~a not found" name v)))))
  (define dir (pkg-dir name v id))
  (unless (and (directory-exists? dir) (file-exists? (hash-file name v id)))
    (log "extracting ~a@~a (~a)" name v (short id))
    (match (source-of name)
      [(? git-src? s) (with-tmp (λ (tmp) (materialize! name v id (git-extract! name s id tmp))))]
      [(? archive-src? s)
       (define want (id->sha1 id))
       (define file
         (if (file-exists? (dl-file want (archive-src-ext s)))
             (dl-file want (archive-src-ext s))
             (let-values ([(file got) (download! s)])
               (unless (equal? got want)
                 (error 'raco-mod
                        "~a@~a: ~a now serves different content (sha1 ~a, want ~a)\n  archive sources keep only their current version; run `raco mod get ~a@latest`"
                        name v (archive-src-url s) got want name))
               file)))
       (with-tmp (λ (tmp)
                   (define root (archive-extract! file (archive-src-ext s) tmp))
                   (define got-v (dir-version name root))
                   (unless (equal? got-v v)
                     (error 'raco-mod "~a: archive ~a contains version ~a, not ~a" name want got-v v))
                   (materialize! name v id root)))]
      [(? dir-src? s)
       (with-tmp (λ (tmp)
                   (define root (dir-fetch! s tmp))
                   (define got (dir-id root))
                   (unless (equal? got id)
                     (error 'raco-mod
                            "~a@~a: ~a now serves different content (~a, want ~a)\n  directory sources keep only their current version; run `raco mod get ~a@latest`"
                            name v (dir-src-url s) got id name))
                   (materialize! name v id root)))]))
  (values dir id (file->string (hash-file name v id))))

;; Like Go's dirhash "h1": sha256 over a sorted manifest of sha256(file) + path.
(define (dir-hash dir)
  (define files
    (sort (for/list ([f (in-directory dir)] #:when (file-exists? f))
            (path->string (find-relative-path dir f)))
          string<?))
  (define manifest
    (apply string-append
           (for/list ([f (in-list files)])
             (format "~a  ~a\n"
                     (bytes->hex-string (call-with-input-file (build-path dir f) sha256-bytes))
                     f))))
  (string-append "h1:" (bytes->string/utf-8
                         (base64-encode (sha256-bytes (open-input-string manifest)) #""))))

;; ---------------------------------------------------------------- gc

(define (path-size p)
  (cond [(link-exists? p) 0]
        [(file-exists? p) (file-size p)]
        [(directory-exists? p)
         (for/sum ([f (in-directory p (λ (d) (not (link-exists? d))))] #:when (file-exists? f))
           (file-size f))]
        [else 0]))

(define (make-writable! dir)
  (run! "chmod" (exe "chmod") (list "-R" "u+w" (path->string dir))))

(define (remove-path! p)
  (cond [(link-exists? p) (delete-file p)]
        [(directory-exists? p) (make-writable! p) (delete-directory/files p)]
        [(file-exists? p) (delete-file p)]))

;; Remove what no live (name ver id) needs: extracted packages, downloaded
;; archives, and git mirrors, plus leftovers of interrupted work. Archive and
;; directory versions that go are also dropped from their index, since they
;; cannot be fetched again; git versions stay, their mirror can be recloned.
;; Mirrors are kept whenever a live package's source is not known locally.
;; -> (listof path) removed (or, with dry-run?, that would be)
(define (gc-cache! live #:dry-run? [dry? #f])
  (define (sub d) (build-path (cache-root) d))
  (define (entries d) (if (directory-exists? (sub d)) (directory-list (sub d)) '()))
  (define keep-pkg (for/set ([l live]) (format "~a@~a+~a" (first l) (second l) (short (third l)))))
  (define keep-dl (for/set ([l live] #:when (regexp-match? #rx"^sha1:" (third l))) (id->sha1 (third l))))
  (define keep-vcs
    (let/ec give-up
      (for*/set ([n (remove-duplicates (map first live))]
                 [s (in-value (if (file-exists? (build-path (cache-root) "source" (string-append n ".rktd")))
                                  (source-of n)
                                  (give-up #f)))]
                 #:when (git-src? s))
        (path->string (file-name-from-path (mirror-dir (git-src-url s)))))))
  (define pkg-garbage
    (for/list ([e (entries "pkg")]
               #:unless (set-member? keep-pkg (regexp-replace #rx"[.]h1$" (path->string e) "")))
      (build-path (sub "pkg") e)))
  (define dl-garbage
    (for/list ([e (entries "dl")]
               #:unless (set-member? keep-dl (car (string-split (path->string e) "."))))
      (build-path (sub "dl") e)))
  (define vcs-garbage
    (if keep-vcs
        (for/list ([e (entries "vcs")] #:unless (set-member? keep-vcs (path->string e)))
          (build-path (sub "vcs") e))
        '()))
  (unless dry?
    (for-each remove-path! (append pkg-garbage dl-garbage vcs-garbage))
    ;; forget archive/directory versions whose content is gone
    (for ([e (entries "index")] #:when (regexp-match? #rx"^[^.]+[.]rktd$|[.]rktd$" (path->string e))
          #:unless (regexp-match? #rx"[.]seen[.]rktd$" (path->string e)))
      (define name (regexp-replace #rx"[.]rktd$" (path->string e) ""))
      (define f (index-file name))
      (define idx (file->value f))
      (when (list? idx)
        (define kept
          (filter (λ (en) (or (not (list? en))
                              (not (regexp-match? #rx"^(sha1|dir):" (e-id en)))
                              (directory-exists? (pkg-dir name (e-ver en) (e-id en)))))
                  idx))
        (unless (equal? kept idx)
          (hash-remove! index-memo name)
          (if (null? kept)
              (delete-file f)
              (write-atomically kept f))))))
  (append pkg-garbage dl-garbage vcs-garbage))

(module+ test
  (require rackunit)

  (define (parsed src) (call-with-values (λ () (parse-git-source src)) list))

  (test-case "parse-git-source"
    (check-equal? (parsed "git://github.com/lexi-lambda/threading?path=threading-lib")
                  '("https://github.com/lexi-lambda/threading" "threading-lib" #f))
    (check-equal? (parsed "https://github.com/u/r.git?path=/a/b/#dev")
                  '("https://github.com/u/r.git" "a/b" "dev"))
    (check-equal? (parsed "github://github.com/u/r/main/sub")
                  '("https://github.com/u/r" "sub" "main"))
    (check-equal? (parsed "git+file:///tmp/x") '("file:///tmp/x" "" #f))
    (check-exn #rx"unsupported package source" (λ () (parse-git-source "some-pkg-name"))))

  (test-case "classify: archives by type, everything else as git"
    (check-equal? (classify "https://download.rhombus-lang.org/releases/current/pkgs/rhombus.zip" "ab")
                  (archive-src "https://download.rhombus-lang.org/releases/current/pkgs/rhombus.zip" "ab" "zip"))
    (check-equal? (archive-src-ext (classify "https://example.com/x.tgz" #f)) "tgz")
    (check-equal? (classify "file:///tmp/x.zip" #f) (archive-src "file:///tmp/x.zip" #f "zip"))
    (check-pred git-src? (classify "git://github.com/lexi-lambda/threading?path=threading-lib" #f))
    (check-equal? (archive-src-ext (classify "https://example.com/x.plt" #f)) "plt")
    (check-equal? (classify "https://example.com/pkgs/x/" "cs") (dir-src "https://example.com/pkgs/x/" "cs" #f))
    ;; like raco pkg, a non-GitHub URL without ".git" is a remote directory
    (check-pred dir-src? (classify "https://gitlab.com/u/r" #f))
    (check-pred git-src? (classify "https://gitlab.com/u/r.git" #f)))

  (test-case "safe-entry? rejects escaping paths"
    (check-true (safe-entry? #"a/b.rkt"))
    (check-false (safe-entry? #"../evil"))
    (check-false (safe-entry? #"a/../../evil"))
    (check-false (safe-entry? #"/etc/passwd")))

  (test-case "info-text-version reads without evaluating"
    (check-equal? (info-text-version "#lang info\n(define deps '())\n(define version \"1.2\")") "1.2")
    (check-equal? (info-text-version "(module info setup/infotab (define version \"0.3\"))") "0.3")
    ;; the form found in built packages
    (check-equal? (info-text-version "(module info setup/infotab (#%module-begin (define deps '()) (define version \"1.1\")))") "1.1")
    (check-false (info-text-version "#lang info\n(define deps '())"))
    (check-false (info-text-version "#lang info\n(define version")))

  (test-case "dir-hash depends on content and paths only"
    (define (mk files)
      (define d (make-temporary-directory))
      (for ([f files])
        (make-parent-directory* (build-path d (car f)))
        (display-to-file (cdr f) (build-path d (car f))))
      d)
    (define a (mk '(("x.rkt" . "1") ("sub/y.rkt" . "2"))))
    (define b (mk '(("sub/y.rkt" . "2") ("x.rkt" . "1"))))
    (define c (mk '(("x.rkt" . "1") ("sub/y.rkt" . "3"))))
    (define d (mk '(("z.rkt" . "1") ("sub/y.rkt" . "2"))))
    (check-equal? (dir-hash a) (dir-hash b))
    (check-not-equal? (dir-hash a) (dir-hash c))
    (check-not-equal? (dir-hash a) (dir-hash d))
    (check-regexp-match #rx"^h1:" (dir-hash a))
    (for-each delete-directory/files (list a b c d))))
