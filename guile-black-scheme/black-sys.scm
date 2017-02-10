

(define head car)
(define (tail x) (force (cdr x)))
;;(define (force x) (x))
;;(define (delay x) (lambda () x))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define scheme-apply apply)
(load "env.scm")
(load "black.scm")

