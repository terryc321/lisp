

;; or macro


;; (or)
;; (or x)
;; (or x y)
;; (or x y z)
(install-macro
 'or
 (lambda (exp)   
   (letrec ((or-helper
	     (lambda (exp)
	       (cond
		;; ( z ) 
		((null? (cdr exp))
		 (let ((gs (gensym)))
		   `(let ((,gs ,(car exp)))
		      (if ,gs ,gs #f))))
		;; ( x ... )
		(else
		 (let ((gs (gensym)))
		   `(let ((,gs ,(car exp)))
		      (if ,gs ,gs ,(or-helper (cdr exp))))))))))
     (cond
      ;; (or) = #f
      ((null? (cdr exp)) #f)
      ;; (or x) = x 
      ((null? (cdr (cdr exp)))
       (car (cdr exp)))
      ;; (or x y)
      ;; (or x y z)
      ;; (or x y z p) ...
      (else 
       (or-helper (cdr exp)))))))





