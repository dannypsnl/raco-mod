#lang racket/base
;; Several raco mod processes may share the cache and a project: readers must
;; never see a half-written file, so every file is written beside its target
;; and renamed into place.
(require racket/file)
(provide write-atomically)

;; display? : display v instead of writing it
(define (write-atomically v f #:display? [display? #f])
  (make-parent-directory* f)
  (define-values (dir _name _d?) (split-path (path->complete-path f)))
  (define tmp (make-temporary-file "tmp-~a" #f dir))
  (with-handlers ([exn:fail? (λ (e) (delete-file tmp) (raise e))])
    (call-with-output-file tmp #:exists 'truncate
      (λ (o) (if display? (display v o) (write v o))))
    (rename-file-or-directory tmp f #t)))
