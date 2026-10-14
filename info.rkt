#lang info
(define collection "raco-mod")
(define version "0.1")
(define deps '("base"))
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/raco-mod.scrbl" () (tool))))
(define raco-commands
  '(("mod" (submod raco-mod/main main) "per-project dependencies with MVS" #f)))
