#lang racket/base
;; racket warm.rkt <root> <log-file>, module paths on stdin: sync's warm build,
;; run in the project's environment. Modules that fail are listed in <log-file>.
(require racket/future racket/string racket/list racket/path setup/parallel-build)

(define (log fmt . args) (eprintf "raco mod: ~a\n" (apply format fmt args)))

(define (warm! root files log-file)
  (define total (length files))
  (define jobs (max 1 (processor-count)))
  (define tty? (terminal-port? (current-error-port)))
  (define width (or (string->number (or (getenv "COLUMNS") "")) 80))
  (define finished 0)
  (define skipped '())
  (define (draw! path)
    (when tty?
      (define line
        (format "raco mod: [~a/~a] ~a" finished total
                (find-relative-path root path)))
      (eprintf "\r\e[K~a"
               (if (> (string-length line) (sub1 width)) (substring line 0 (sub1 width)) line))))
  (unless tty? (log "compiling ~a modules with ~a workers" total jobs))
  (define start (current-inexact-milliseconds))
  (parallel-compile-files
    files
    #:worker-count jobs
    #:handler (λ (_id kind path msg out err)
                (case kind
                  [(start) (draw! path)]
                  [(done) (set! finished (add1 finished)) (draw! path)]
                  [(error)
                   (set! finished (add1 finished))
                   (set! skipped (cons (cons path (without-context msg)) skipped))
                   (draw! path)])))
  (when tty? (eprintf "\r\e[K"))
  (define secs (real->decimal-string (/ (- (current-inexact-milliseconds) start) 1000.0) 1))
  (cond
    [(null? skipped)
     (when (file-exists? log-file) (delete-file log-file))
     (log "compiled ~a project modules and their dependencies in ~as" total secs)]
    [else
     (with-output-to-file log-file #:exists 'truncate/replace
       (λ () (for ([s (reverse skipped)]) (printf "~a\n~a\n\n" (car s) (cdr s)))))
     (log "compiled ~a project modules and their dependencies in ~as; ~a failed, see ~a"
          (- total (length skipped)) secs (length skipped) log-file)]))

;; the error itself, without the stack traces
(define (without-context msg)
  (string-join (takef (string-split (format "~a" msg) "\n")
                      (λ (l) (not (regexp-match? #rx"^ *(compilation )?context[.][.][.]:" l))))
               "\n"))

(module+ main
  (define root (string->path (vector-ref (current-command-line-arguments) 0)))
  (define log-file (vector-ref (current-command-line-arguments) 1))
  (define files
    (for/list ([l (in-lines (current-input-port))] #:unless (string=? l "")) (string->path l)))
  (unless (null? files) (warm! root files log-file)))
