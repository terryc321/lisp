
(define scheme-apply apply)
(load "env.scm")
(load "interpreter.scm")

(define (go)
  (run init-env 0 'ok))

