
(define directory "/home/terry/lisp/cps-interpreter/")

(define scheme-apply apply)
(load (string-append "env.scm"))
(load (string-append "black.scm"))
;; or (load "black-with-delta.scm")
(black)







