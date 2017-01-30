
;; (load "black.scm")
;; (black)
;; 

(exec-at-metalevel (let ((old-eval base-eval))
		     (set! base-eval (lambda (exp env cont)
				       (write exp) (newline)
				       (old-eval exp env cont)))))





