#lang info
(define collection "raco-mod")
(define deps '("base"))
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/raco-mod.scrbl" () (tool))))
(define version "0.1")
(define license '(Apache-2.0 OR MIT))
(define pkg-authors '(dannypsnl))

(define raco-commands
  '(("mod" (submod raco-mod/main main) "per-project dependencies with MVS" #f)))
