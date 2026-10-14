#lang racket/base
;; Rewrite the `deps` definition of an info.rkt in place, leaving the rest of
;; the file untouched.
(require racket/file racket/list racket/match racket/string syntax/parse "atomic.rkt")
(provide update-deps! set-dep-version)

;; f : current deps (as read) -> new deps
(define (update-deps! path f)
  (define text (file->string path))
  (define stx
    (parameterize ([read-accept-reader #t] [read-accept-lang #t])
      (define in (open-input-string text))
      (port-count-lines! in)
      (read-syntax path in)))
  (define form (find-define stx 'deps))
  (define old (if form (eval-deps (third (syntax->list form))) '()))
  (define new-text (render (f old)))
  (define out
    (if form
        (let ([start (sub1 (syntax-position form))])
          (string-append (substring text 0 start)
                         new-text
                         (substring text (+ start (syntax-span form)))))
        (string-append (string-trim text #:left? #f) "\n" new-text "\n")))
  (write-atomically out path #:display? #t))

;; A deps entry with its #:version set to v; other keywords such as
;; #:platform are kept as they are.
(define (set-dep-version d name v)
  (define (set kws)
    (match kws
      ['() (list '#:version v)]
      [(list* '#:version _ rest) (list* '#:version v rest)]
      [(list* k x rest) (list* k x (set rest))]))
  (match d
    [(? string?) (list name '#:version v)]
    [(cons s kws) (cons s (set kws))]))

;; read, not expanded: there are no bindings, so match `define` by datum
(define (find-define stx name)
  (let loop ([s stx])
    (syntax-parse s
      [((~datum define) n:id _) #:when (eq? (syntax-e #'n) name) s]
      [(x ...) (for/or ([x (in-list (attribute x))]) (loop x))]
      [_ #f])))

;; deps is almost always a quoted literal; `list` of literals is handled too
(define (eval-deps stx)
  (match (syntax->datum stx)
    [(list 'quote v) v]
    [(list 'list xs ...) (map eval-deps* xs)]
    [v (error 'raco-mod "cannot edit non-literal deps: ~s" v)]))
(define (eval-deps* d)
  (match d
    [(list 'quote v) v]
    [(? string? s) s]
    [_ (error 'raco-mod "cannot edit non-literal deps entry: ~s" d)]))

(define (render deps)
  (define indent (make-string (string-length "(define deps '(") #\space))
  (string-append "(define deps '("
                 (string-join (map (λ (d) (format "~s" d)) deps) (string-append "\n" indent))
                 "))"))

(module+ test
  (require rackunit)

  (define (edited text f)
    (define p (make-temporary-file "info~a.rkt"))
    (display-to-file text p #:exists 'truncate/replace)
    (update-deps! p f)
    (begin0 (file->string p) (delete-file p)))

  (test-case "replaces only the deps form"
    (define out
      (edited "#lang info\n;; keep me\n(define collection \"app\")\n(define deps '(\"base\" (\"a\" #:version \"1.0\")))\n(define x 1)\n"
              (λ (ds) (append ds '(("b" #:version "2.0"))))))
    (check-equal? out
                  (string-append "#lang info\n;; keep me\n(define collection \"app\")\n"
                                 "(define deps '(\"base\"\n               (\"a\" #:version \"1.0\")\n"
                                 "               (\"b\" #:version \"2.0\")))\n(define x 1)\n")))

  (test-case "appends deps when missing"
    (check-equal? (edited "#lang info\n(define collection \"app\")\n" (λ (ds) (cons "base" ds)))
                  "#lang info\n(define collection \"app\")\n(define deps '(\"base\"))\n"))

  (test-case "understands (list ...) deps"
    (check-regexp-match #rx"\\(define deps '\\(\"base\"\\)\\)"
                        (edited "#lang info\n(define deps (list \"base\" \"gone\"))\n"
                                (λ (ds) (remove "gone" ds)))))

  (test-case "set-dep-version keeps other keywords"
    (check-equal? (set-dep-version "a" "a" "1.0") '("a" #:version "1.0"))
    (check-equal? (set-dep-version '("a" #:platform macosx) "a" "1.0")
                  '("a" #:platform macosx #:version "1.0"))
    (check-equal? (set-dep-version '("a" #:version "0.1" #:platform macosx) "a" "1.0")
                  '("a" #:version "1.0" #:platform macosx))
    (check-equal? (set-dep-version '("a" #:platform macosx #:version "0.1") "a" "1.0")
                  '("a" #:platform macosx #:version "1.0")))

  (test-case "refuses computed deps"
    (check-exn #rx"non-literal"
               (λ () (edited "#lang info\n(define deps (append a b))\n" values)))))
