#lang racket/base
;; Minimal Version Selection (Russ Cox, "Minimal Version Selection", 2018).
;;
;; The graph is given implicitly by `reqs-of`, which maps a node to its edges.
;; A node is 'root or (cons name ver); an edge is (cons name minimum-ver).
(require racket/set racket/match)
(provide (struct-out result) build-list why)

;; selected : hash name -> ver
;; graph    : hash node -> (listof (cons name ver)), every node visited
(struct result (selected graph) #:transparent)

(define (build-list reqs-of ver<?)
  (define graph (make-hash))
  (define selected (make-hash))
  (let visit ([node 'root])
    (unless (hash-has-key? graph node)
      (define es (reqs-of node))
      (hash-set! graph node es)
      (for ([e (in-list es)])
        (match-define (cons name ver) e)
        (hash-update! selected name (λ (old) (if (ver<? old ver) ver old)) ver)
        (visit e))))
  (result selected graph))

;; Shortest path from root to the selected version of `name`, following only
;; edges that require exactly the selected version (i.e. that explain it).
(define (why res name)
  (define sel (result-selected res))
  (define (explains? e)
    (match-define (cons name ver) e)
    (equal? (hash-ref sel name #f) ver))
  (let loop ([frontier (list (list 'root))] [seen (set 'root)])
    (cond
      [(null? frontier) #f]
      [(for/first ([path (in-list frontier)]
                   #:when (match (car path) [(cons n _) (equal? n name)] [_ #f]))
         (reverse path))]
      [else
       (define-values (next seen*)
         (for*/fold ([next '()] [seen seen])
                    ([path (in-list frontier)]
                     [e (in-list (hash-ref (result-graph res) (car path) '()))]
                     #:when (and (explains? e) (not (set-member? seen e))))
           (values (cons (cons e path) next) (set-add seen e))))
       (loop (reverse next) seen*)])))

(module+ test
  (require rackunit version/utils)

  ;; The example graph from https://research.swtch.com/vgo-mvs (A is the root).
  (define g
    (hash 'root '(("B" . "1.2") ("C" . "1.2"))
          '("B" . "1.1") '(("D" . "1.1"))
          '("B" . "1.2") '(("D" . "1.3"))
          '("C" . "1.2") '(("D" . "1.4"))
          '("C" . "1.3") '(("F" . "1.1"))
          '("D" . "1.1") '(("E" . "1.1"))
          '("D" . "1.3") '(("E" . "1.2"))
          '("D" . "1.4") '(("E" . "1.2"))
          '("E" . "1.2") '()))

  (define res (build-list (λ (n) (hash-ref g n '())) version<?))

  (test-case "MVS picks the max of the reachable minimums"
    (check-equal? (result-selected res)
                  (make-hash '(("B" . "1.2") ("C" . "1.2") ("D" . "1.4") ("E" . "1.2")))))

  (test-case "unreached versions are never visited"
    (check-false (hash-has-key? (result-graph res) '("C" . "1.3"))))

  (test-case "why follows edges that explain the selected version"
    (check-equal? (why res "E") '(root ("C" . "1.2") ("D" . "1.4") ("E" . "1.2")))
    (check-false (why res "F"))))
