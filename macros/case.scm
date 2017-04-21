

;; case
;; case key clause clause ...
;;     Key may be any expression. Each clause has this form:
;;               ((object ...) expression expression ...)
;;     No object is evaluated, and all the objects must be distinct. The last clause may be an else clause, which has the form:
;;               (else expression expression ...)
;;     A case expression does the following:
;;         Evaluates key and compares the result with each object.
;;         If the result of evaluating key is equivalent (in the sense of eqv?; see Equivalence Predicates) to an object, case evaluates the expressions in the corresponding clause from left to right and returns the result of evaluating the last expression in the clause as the result of the case expression.
;;         If the result of evaluating key is different from every object, and if there's an else clause, case evaluates its expressions and returns the result of the last one as the result of the case expression. If there's no else clause, case returns an unspecified result. Programs should not depend on the value of a case expression that has no else clause. 
;;     For example,
;;               (case (* 2 3)
;;                  ((2 3 5 7) 'prime)
;;                  ((1 4 6 8 9) 'composite))            =>  composite
;;               (case (car '(c d))
;;                  ((a) 'a)
;;                  ((b) 'b))                            =>  unspecified
;;               (case (car '(c d))
;;                  ((a e i o u) 'vowel)
;;                  ((w y) 'semivowel)
;;                  (else 'consonant))                   =>  consonant
         
(define (f exp)
  (let ((val (car (cdr expr))))
    `(cond      
      (or

;; ((2 3 5 7) ....) -> (or (eqv? (begin ...)

       (define (helper-not-last case-expression)	
	 (let ((objs (car case-expression))
	       (objs-body (cdr case-expression)))	     
	   (list (cons 'or (map (lambda (obj) `(eqv? ,obj val)) objs))
		 `(begin ,@objs-body))))

      

       
       
       
       
       

       
			      


(install-macro
 'case
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




