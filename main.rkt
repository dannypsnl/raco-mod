#lang racket/base
;; raco mod -- per-project Racket dependencies with Minimal Version Selection,
;; driven only by info.rkt.
(require racket/match
         racket/system
         setup/dirs
         "private/mvs.rkt"
         "private/project.rkt"
         "private/pkginfo.rkt")

(define usage #<<EOF
usage: raco mod <command> [args]

  sync                 resolve info.rkt deps with MVS, write info.sum and .rkt/ links
  get pkg[@ver] ...    set #:version in info.rkt deps (ver: X.Y | latest | none), then sync
  list                 print the build list
  graph                print the requirement graph
  why pkg              show the requirement chain that selects pkg's version
  env                  print environment settings for this project
  exec cmd args ...    sync, then run cmd inside this project's environment
  drracket args ...    sync, then run DrRacket in this project, keeping user-scope tools
  clean [-n]           delete cached packages and compiled code no project uses (-n: only list)
EOF
  )

(define (racket-bin name)
  (define p (build-path (find-console-bin-dir) name))
  (if (file-exists? p) p (find-executable-path name)))

;; run a Racket executable with these environment variables, exiting with its code
(define (run-in vars cmd args)
  (define exe (or (racket-bin cmd) (error 'raco-mod "command not found: ~a" cmd)))
  (define env (environment-variables-copy (current-environment-variables)))
  (for ([kv vars])
    (environment-variables-set! env (string->bytes/utf-8 (car kv)) (string->bytes/utf-8 (cdr kv))))
  (exit (parameterize ([current-environment-variables env])
          (apply system*/exit-code exe args))))

(define (show node)
  (if (eq? node 'root) "info.rkt" (format "~a@~a" (car node) (cdr node))))

(define (main args)
  (match args
    [(list "sync") (void (sync! (find-project-root)))]
    [(list "get" specs ..1) (void (get! (find-project-root) specs))]
    [(list "list")
     (define root (find-project-root))
     (define sel (result-selected (build-res (load-build root))))
     (define direct (map car (pkg-deps root)))
     (for ([n (sort (hash-keys sel) string<?)])
       (printf "~a ~a~a\n" n (hash-ref sel n) (if (member n direct) "" "  // indirect")))]
    [(list "graph")
     (define res (build-res (load-build (find-project-root))))
     (for* ([node (cons 'root (for/list ([(n v) (in-hash (result-selected res))]) (cons n v)))]
            [e (in-list (hash-ref (result-graph res) node '()))])
       (printf "~a ~a\n" (show node) (show e)))]
    [(list "why" name)
     (match (why (build-res (load-build (find-project-root))) name)
       [#f (printf "~a is not in the build list\n" name)]
       [p (for ([n p]) (displayln (show n)))])]
    [(list "clean" flags ...)
     (define dry? (member "-n" flags))
     (define-values (removed bytes live) (clean! #:dry-run? (and dry? #t)))
     (for ([d removed]) (printf "~a ~a\n" (if dry? "would remove" "removed") d))
     (printf "~a ~a unused cache entr~a (~a MB); ~a project~a in use\n"
             (if dry? "would free" "freed") (length removed) (if (= 1 (length removed)) "y" "ies")
             (real->decimal-string (/ bytes 1048576.0) 1)
             (length live) (if (= 1 (length live)) "" "s"))]
    [(list "env")
     (define fish? (regexp-match? #rx"fish" (or (getenv "SHELL") "")))
     (for ([kv (project-env (find-project-root))])
       (if fish?
           (printf "set -gx ~a ~s;\n" (car kv) (cdr kv))
           (printf "export ~a=~s\n" (car kv) (cdr kv))))]
    [(list "exec" cmd args ...)
     (define root (find-project-root))
     (sync! root)
     (run-in (project-env root) cmd args)]
    [(list "drracket" args ...)
     (define root (find-project-root))
     (run-in (drracket-env! root (sync! root)) "drracket" args)]
    [_ (displayln usage (current-error-port)) (exit 2)]))

(module+ main
  (with-handlers ([exn:fail? (λ (e) (eprintf "~a\n" (exn-message e)) (exit 1))])
    (main (vector->list (current-command-line-arguments)))))
