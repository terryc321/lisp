

(define quote (fexpr (n) n))

;; (define apply
;;   (fexpr (f . args)
;; 	 (display "current env = ")
;; 	 (display (current-environment))
;; 	 (newline)
;; 	 (eval (cons f args))))








;; this doesnt even set the variable to value
;; (define set (fexpr (var-expr val-expr)
;; 		   (display "var-expr = ") (display var-expr) (newline)
;; 		   (display "val-expr = ") (display val-expr) (newline)		     
;; 		   (let ((var (eval var-expr))
;; 			 (val (eval val-expr)))
;; 		     (display "var = ") (display var) (newline)
;; 		     (display "val = ") (display val) (newline)		     
;; 		     (set! var val)
;; 		     (newline)
;; 		     (display "env.cdr = ")
;; 		     (display (cdr (current-environment)))
;; 		     (newline)	     		     
;; 		     val)))

;; b - > a 
;; (set b 123)
;;  same as (setq a 123)
;; (define set (fexpr (var-expr val-expr)
;; 		   (let ((s (eval var-expr))
;; 			 (v (eval val-expr))
;; 			 (ce (current-environment)))
;; 		     (display "ce = ") (display (cdr ce)) (newline)
;; 		     (letrec ((set-helper (lambda (xs)
;; 					    (cond
;; 					     ;; no more symbols
;; 					     ((null? xs) #f)
;; 					     ;; no value to alter
;; 					     ((null? (cdr xs)) #f)
;; 					     ;; 
;; 					     ((eq? s (car xs))
;; 					      (set-car! (cdr xs) v)
;; 					      v)
;; 					     (else
;; 					      (set-helper (cdr xs)))))))
;; 		       ;; current environment has bindings
;; 		       ;; introduced by fexpr
;; 		       ;; drop 2 bindings need 4 cdrs  + 1 cdr to drop environment header
;; 		       (set-helper (cdr (cdr (cdr (cdr
;; 						   (cdr ce))))))))))


(define drop-binding (lambda (n xs)
		       (cond
			((null? xs) xs)
			((null? (cdr xs)) xs)
			((< n 1) xs)
			(else (drop-binding (- n 1) (cdr (cdr xs)))))))



;; drop binding of the s v in the params of the actual SET itself
;; then we get to original environment
;; but the current environment of function is a closure
;; -- so we need to pass in callsite environment
;; (define set (lambda (s v)
;; 	      (let ((ce (drop-binding 2 (current-environment))))
;; 		(display "ce = ") (display (cdr ce)) (newline)
;; 		(letrec ((set-helper (lambda (xs)
;; 				       (cond
;; 					;; no more symbols
;; 					((null? xs) #f)
;; 					;; no value to alter
;; 					((null? (cdr xs)) #f)
;; 					;; 
;; 					((eq? s (car xs))
;; 					 (set-car! (cdr xs) v)
;; 					 v)
;; 					(else
;; 					 (set-helper (cdr xs)))))))
;; 		  ;; current environment has bindings
;; 		  ;; introduced by fexpr
;; 		  ;; drop 2 bindings need 4 cdrs  + 1 cdr to drop environment header
;; 		  (set-helper (cdr (cdr
;; 				    (cdr (cdr
;; 					  (cdr ce))))))))))




(define set (lambda (s v caller-env)
	      (display "caller-env = ") (display (cdr caller-env)) (newline)
	      (display "set closure-env = ") (display (cdr (current-environment))) (newline)	      
	      (letrec ((set-helper (lambda (xs)
				     (cond
					;; no more symbols
				      ((null? xs) #f)
				      ;; no value to alter
				      ((null? (cdr xs)) #f)
				      ;; 
				      ((eq? s (car xs))
				       (set-car! (cdr xs) v)
				       v)
				      (else
				       (set-helper (cdr xs)))))))
		;; current environment has bindings
		;; introduced by fexpr
		;; drop 2 bindings need 4 cdrs  + 1 cdr to drop environment header
		(set-helper (cdr caller-env)))))










;; handle on environment and able to manipulate it , understand how binding affects environment
;; can then write procedures like SET SET! DEFINE
;; interestingly these new dedfinitions can be debugged more easily because its hosted and
;; we control how and when evaluation happens














    
      
    
    








			 


;; some environment helpers



;; works on lists , and anything else
;; gets caught out by circular lists
;; tail recursive -good.
(define length
  (lambda (xs)
    (letrec ((length-acc
	      (lambda (ys acc)
		(cond
		 ((null? ys) acc)
		 ((pair? ys) (length-acc (cdr ys) (+ 1 acc)))
		 (else (+ 1 acc))))))
      (length-acc xs 0))))


;; (append)
;; (append '(1 2 3))
;; (append '(1 2 3) '(4 5 6))
;; (append '(1 2 3) '(4 5 6) '(7 8 9))
;; (append '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12))
(define append
  (lambda args
    (letrec ((app2 (lambda (xs ys)
		     (cond
		      ((null? xs) ys)
		      (else (cons (car xs)
				  (app2 (cdr xs) ys))))))
	     (app (lambda (xs)
		    (cond
		     ;; no lists
		     ((null? xs) '())
		     ;; 1st list only then
		     ((null? (cdr xs)) (car xs))
		     ;; more than 1 list
		     (else (app2 (car xs)
				 (app (cdr xs))))))))
      (app args))))


;; (reverse)
(define reverse
  (lambda (xs)
    (letrec ((rev-acc (lambda (ys acc)
			(cond
			 ((null? ys) acc)
			 ((pair? ys) (rev-acc (cdr ys) (cons (car ys) acc)))
			 (else acc)))))
      (rev-acc xs '()))))

;;
(define square
  (lambda (n) (* n n)))

;; (map)
(define map
  (lambda (f . args)
    (list f args)))


;; load other libraries also from this.
(load "tests/fib.scm")









      
		    
