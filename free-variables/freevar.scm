
;; find free variables of a routine


;; **TODO**
;; aha that old chestnut

;; set union
(define (set-union xs ys)
  (cond
   ((null? xs) ys)
   ((member (car xs) ys)
    (set-union (cdr xs) ys))
   (else (set-union (cdr xs) (cons (car xs) ys)))))

;; is symbol bound in environment
(define (bound? sym env)  (member sym env))


;; free-vars takes an expression and list of bound variables 
(define (free-vars exp env cont)
  (cond
   ;; empty list is constant - no free variables
   ((null? exp) (cont '()))
   ;; booleans - no free variables 
   ((boolean? exp) (cont '()))
   ;; numbers - no free variables
   ((number? exp) (cont '()))
   ;; symbols
   ((symbol? exp) (if (bound? exp env) (cont '())  (cont (list exp))))
   ;; lambda abstractions
   ((and (pair? exp)
	 (eq? (car exp) 'lambda))
    ;; its a lambda abstraction !!
    (free-vars-lambda (car (cdr exp)) (cdr (cdr exp)) env cont))
   
   ;; applications
   ((pair? exp)
    (free-vars-application (car exp) (cdr exp) env cont))
   (else (error "free-vars : dont know how handle this " exp env))))

(define (free-vars-lambda bind body env cont)
  (free-vars body (append bind env)
	     (lambda (fbody)
	       (format #t "~% ~a : ~a ~%" fbody body)
	       (cont fbody))))



 
(define (free-vars-application fun args env cont)
  (free-vars fun env (lambda (ff)
		       (free-vars-application-args args '() env
						   (lambda (fargs)
						     (cont (set-union ff fargs)))))))

(define (free-vars-application-args args fset env cont)
  (cond
   ((null? args) (cont fset))
   (else (free-vars (car args) env (lambda (farg)
				     (free-vars-application-args (cdr args)
								 (set-union farg fset)
								 env
								 cont))))))




;;
			

(free-vars 	       
'(lambda (x y z k)
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
      (lambda () (k z))))
'()
(lambda (x) x))


(define (fv exp)
  (let ((ff (free-vars exp '() (lambda (x) x))))
    (display (format #f "~%~%"))
    ff))


(fv '(lambda () x)) ;; x is free in this expression
(fv '(lambda (x) x))  ;; no free vars
(fv '(lambda (x) (+ x y))) ;; + y are free in expression












   


