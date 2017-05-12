;; alpha conversion renaming

;; AVOID use of SET! at ALL costs , because intend to implement
;; in a functional theorem prover style language , maybe ocaml.

;; every symbol gets a unique identifier
;; each unique symbol is replaced by the same unique identifier as determined by scope
;; initially assuming environment is empty ? or doesnt it matter ?

;; cont now takes 3 objects : object new-env new-gensyms 
;; absolutely utterly brilliant use of continuation passing style


;; primiitve language
;; define , begin , set! 

;; gensyms are ((a 1)(b 2)(c 1))
;; so when we call ac-gensym
;; we should also make a suitable entry in gensyms (gensyms) also 
;;(assoc 'a '((a 1)(b 2)))
;;(define (ac-update-gensym) #f)


;; put a symbol and a integer and punt out a symbol , simples.
(define (ac-gensym sym n)
  (string->symbol (string-append (symbol->string sym)
				 "_"
				 (number->string n))))


(define (ac-define? exp)
  (and (pair? exp)
       (eq? (car exp) 'define)))

(define (ac-lambda? exp)
  (and (pair? exp)
       (eq? (car exp) 'lambda)))


(define (ac-begin? exp)
  (and (pair? exp)
       (eq? (car exp) 'begin)))


(define (ac-constant? exp)
  (or (null? exp)
      (number? exp)
      (boolean? exp)
      (char? exp)
      (string? exp)))



(define (ac-bound? exp env)
  (and (symbol? exp)
       (assoc exp env)))



(define (ac-extend-env keys vals env)
  (cond
   ((null? keys) env)
   (else (cons (cons (car keys) (car vals))
	       (ac-extend-env (cdr keys)
			      (cdr vals)
			      (cons (car keys) (cons (car vals) '())))))))


;; if given symbol is in the list , 


;;  symbol -> gensyms  ->  \ a . b [ new-symbol  new-gensyms ] ....
(define (ac-gensym-1 sym gens kab)
  (let ((var-n (assoc sym gens)))
    (if var-n
	;; 
	(begin
	  ;; (display "ac-gensym-1 : n = ")
	  ;; (display n)
	  ;; (newline)
	  (let ((n2 (+ 1 (cdr var-n))))
	    (kab (ac-gensym sym n2)
		 (cons (cons sym n2) gens))))
	;; 
	(begin 
	  (kab (ac-gensym sym 1) ;; sym
	       (cons (cons sym 1) gens))))))







(define (ac exp env gens cont)
  (cond   
   ;; if its a symbol , then it must be either bound OR toplevel
   ((symbol? exp)
    (if (ac-bound? exp env)
	(begin
	  (cont (cdr (assoc exp env)) env gens))
	(begin
	  ;; (let ((gs (make-unique exp)))
	  ;;   (cont gs
	  ;; 	  (cons (cons exp gs) env)
	  ;; 	  gens))
	  (ac-gensym-1 exp gens (lambda (new-sym new-gensyms)
				  (cont new-sym
					(cons (cons exp new-sym) env)
					new-gensyms))))))
   
   ;; environment unchanged no binding to see here -- move on
   ((ac-constant? exp) (cont exp env gens))
   ((ac-lambda? exp) (ac-lambda exp env gens cont))
   ((ac-begin? exp) (ac-begin exp env gens cont))
   ((ac-define? exp) (ac-define exp env gens cont))
   ((pair? exp) (ac-app exp env gens cont))
   (else
    (error "dont know how to process this " exp env gens cont))))





;; ensure binding is in order for lambda left to right
;; for each binding , make a unique symbol
;; then reverse them at end ...
;; vars -> new-vars -> new-gensyms -> \ a . b  [ new-vars new-gensyms ]
;;
;; have to reverse the new-vars 
(define (ac-lambda-vars vars new-vars new-env new-gensyms kab)
  (cond
   ((null? vars) (kab (reverse new-vars) new-env new-gensyms))
   (else (let ((sym (car vars)))
	   (ac-gensym-1 sym new-gensyms
			(lambda (new-sym new-gens3)
			  (ac-lambda-vars (cdr vars)
					  (cons new-sym new-vars)
					  (cons (cons sym new-sym) new-env)
					  new-gens3
					  kab)))))))




;; simple lambda lists
;; (lambda (a b c) ... )
(define (ac-lambda exp env gens cont)
  (let ((vars (car (cdr exp)))
	(body (cdr (cdr exp))))
    (ac-lambda-vars vars
		    '()
		    '()
		    gens
		    (lambda (new-lambda-vars partial-env new-gensyms)
		      ;; (let ((new-bind (map (lambda (x) (cons x (make-unique x))) bindings)))
		      ;;   (let ((new-env (append new-bind env))
		      ;;    (lambda-bind (map cdr new-bind)))
		      ;; (display "new-env = ")
		      ;; (display new-env)
		      ;; (newline)
		      (let ((new-env (append partial-env env)))
			(ac-body body
				 '()
				 new-env
				 new-gensyms
				 (lambda (exp2 env2 gens3)
				   ;; re-install the original environment env 
				   (cont (append (list 'lambda new-lambda-vars)
						 exp2)
					 env
					 gens3))))))))




(define (ac-define exp env gens cont) #f)

  ;; (ac (car (cdr exp)) '() env gens (lambda (exp2 env2 gens2)
  ;; 				     (ac (car (cdr (cdr exp))) env2 gens2
					
  ;; 				    (cont `(define ,@exp2 ,@exp3)
  ;; 					  env2
  ;; 					  gens2))))




(define (ac-body body new-body env gens cont)
  (cond
   ((null? body) (cont new-body env gens))
   (else
    (ac (car body) env gens (lambda (body2 env2 gens2)
			 (ac-body (cdr body)
				  (append new-body (list body2))
				  env2
				  gens2
				  cont))))))



(define (ac-begin exp env gens cont)
  (ac-body (cdr exp) '() env gens (lambda (exp2 env2 gens2)
				    (cont `(begin ,@exp2)
					  env2
					  gens2))))







   
(define (ac-app exp env gens cont)
  (ac-body exp '() env gens cont))
		  


(define (alpha-convert exp)
  (let ((env '())
	(gens '())
	(cont (lambda (exp2 env2 gens2) exp2)))
    (ac exp env gens cont)))




;; **********************************************************************

;; x is free
(define (test1)
  (alpha-convert 'x))

;; ;; no free vars
(define (test2)
  (alpha-convert '(lambda (x) x)))

;; + y are free in expression  
(define (test3)
  (alpha-convert '(lambda (x) (+ x y))))

(define (test4)
  (alpha-convert
   '(define ktak
      (lambda (x y z k)
	(k< y x
	    (lambda () ; (< y x)
	      (k- x 1
		  (lambda (x1) 
		    (ktak x1 y z
			  (lambda (t1)
			    (k- y 1
				(lambda (y1) 
				  (ktak y1 z x
					(lambda (t2)
					  (k- z 1
					      (lambda (z1) 
						(ktak z1 x y
						      (lambda (t3)
							(ktak t1 t2 t3 k))))))))))))))
	    (lambda () (k z)))))))


(define (test5)
  (alpha-convert '(lambda ()
		    (lambda (x) x)
		    (lambda (y) y)
		    (lambda (z)
		      (lambda (x)
			(lambda (y)
			  z)
			x)
		      z))))




(define (test6)
  (alpha-convert '(begin a b c )))


(define (test7)
  (alpha-convert '((define a (a b c))(define y (d e f a y))(define z (a b c))(cons 1 2))))

(define (test8)
  (alpha-convert '(lambda (x) (lambda (x) (lambda (x) x) x) x)))




