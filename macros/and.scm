

;; and macro


;; (and)
;; (and x)
;; (and x y)
;; (and x y z)
(install-macro
 'and
 (lambda (exp)
   (letrec ((and-helper
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
		      (if ,gs ,(and-helper (cdr exp)) #f))))))))
     (cond
      ;; (and) = #t
      ((null? (cdr exp)) #t)
      ;; (and x) = x 
      ((null? (cdr (cdr exp)))
       (car (cdr exp)))
      ;; (and x y)
      ;; (and x y z)
      ;; (and x y z p) ...
      (else
       (and-helper (cdr exp)))))))



