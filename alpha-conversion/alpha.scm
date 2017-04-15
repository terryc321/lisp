;; alpha conversion renaming

;; AVOID use of SET! at ALL costs , because intend to implement
;; in a functional theorem prover style language , maybe ocaml.

;; every symbol gets a unique identifier
;; each unique symbol is replaced by the same unique identifier as determined by scope
;; initially assuming environment is empty ? or doesnt it matter ?

;; cont now takes 2 objects : <object> <new-environment>
;; if 

(define (ac-lambda? exp)
  (and (pair? exp)
       (eq? (car exp) 'lambda)))


(define (ac-constant? exp)
  (or (null? exp)
      (number? exp)
      (boolean? exp)
      (string? exp)))

(define (ac-bound? exp env)
  (and (symbol? exp)
       (assoc exp env)))
		 

(define (make-unique sym)
  (gensym (symbol->string sym)))
  

(define (ac exp env cont)
  (cond
   ;; if its a symbol , then it must be either bound OR toplevel
   ((symbol? exp)
    (if (ac-bound? exp env)
	(begin
	  (cont (cdr (assoc exp env)) env))
	(begin (let ((gs (make-unique exp)))
		 (cont gs (cons (cons exp gs) env))))))
   ;; environment unchanged no binding to see here -- move on
   ((ac-constant? exp) (cont exp env))
   ((ac-lambda? exp) (ac-lambda exp env cont))
   ((pair? exp) (ac-app exp env cont))
   (else
    (error "dont know how to process this " exp env cont))))


(define (ac-extend-env keys vals env)
  (cond
   ((null? keys) env)
   (else (cons (cons (car keys) (car vals))
	       (ac-extend-env (cdr keys)
			      (cdr vals)
			      (cons (car keys) (cons (car vals) '())))))))


;; simple lambda lists
;; (lambda (a b c) ... )
(define (ac-lambda exp env cont)
  (let ((bindings (car (cdr exp)))
	(body (cdr (cdr exp))))
    (let ((new-bind (map (lambda (x) (cons x (make-unique x))) bindings)))
      (let ((new-env (append new-bind env))
	    (lambda-bind (map cdr new-bind)))
	;; (display "new-env = ")
	;; (display new-env)
	;; (newline)
	(ac-body body '() new-env (lambda (exp2 env2)
				    ;; re-install the original environment env 
				    (cont (list 'lambda_ lambda-bind exp2) env)))))))











(define (ac-body body new-body env cont)
  (cond
   ((null? body) (cont new-body env))
   (else
    (ac (car body) env (lambda (body2 env2)
			 (ac-body (cdr body)
				  (append new-body (list body2))
				  env2
				  cont))))))






   
(define (ac-app exp env cont)
  (ac-body exp '() env cont))
		  

(define (alpha-convert exp)
  (let ((env '())
	(cont (lambda (exp2 env_) exp2)))
    (ac exp env cont)))




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
  (alpha-convert '(define ktak
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



