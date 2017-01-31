
;; mit-scheme
;; (load "env.scm")
;; (load "black.scm")
;; (load "int.scm")
;; (load "black.scm") or (load "black-with-delta.scm")
;; (black)
;;  

;; let is not defined in black
;; (exec-at-metalevel (let ((old-eval base-eval))
;; 		     (set! base-eval (lambda (exp env cont)
;; 				       (write exp) (newline)
;; 				       (old-eval exp env cont)))))


;; 
;; rewriting base-eval to write expression we are evaluating
;; then passing it to old-base-eval
(exec-at-metalevel ((lambda (old-eval)
		      (set! base-eval (lambda (exp env cont)
					(write exp)
					(newline)
					(old-eval exp env cont)))) base-eval))



(EM (let ((old-eval base-eval))
      (set! base-eval (lambda (exp env cont)
			(write exp)
			(newline)
			(old-eval exp env cont)))))







