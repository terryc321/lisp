

;; does not seem to work in GUILE



(use-modules (ice-9 pretty-print))
(define pp pretty-print)

(define head car)
(define (tail x) (force (cdr x)))
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define scheme-apply apply)
(load "/home/terry/lisp/black-scheme/env.scm")
(load "/home/terry/lisp/black-scheme/black.scm")
;;or (load "/home/terry/lisp/black-scheme/black-with-delta.scm")
(black)



