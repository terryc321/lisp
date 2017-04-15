
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

;; ;; free-vars takes an expression and list of bound variables 
;; (define (free-vars exp env cont)
;;   (cond
;;    ;; empty list is constant - no free variables
;;    ((null? exp) (cont '()))
;;    ;; booleans - no free variables 
;;    ((boolean? exp) (cont '()))
;;    ;; numbers - no free variables
;;    ((number? exp) (cont '()))
;;    ;; symbols
;;    ((symbol? exp) (if (bound? exp env) (cont '())  (cont (list exp))))
;;    ;; lambda abstractions
;;    ((and (pair? exp)
;; 	 (eq? (car exp) 'lambda))
;;     ;; its a lambda abstraction !!
;;     (free-vars-lambda (car (cdr exp)) (cdr (cdr exp)) env cont))
   
;;    ;; applications
;;    ((pair? exp)
;;     (free-vars-application (car exp) (cdr exp) env cont))
;;    (else (error "free-vars : dont know how handle this " exp env))))


;; (define (free-vars-lambda bind body env cont)
;;   (free-vars body (append bind env)
;; 	     (lambda (fbody)
;; 	       (format #t "~% ~a : ~a ~%" fbody body)
;; 	       (cont fbody))))

 
;; (define (free-vars-application fun args env cont)
;;   (free-vars fun env (lambda (ff)
;; 		       (free-vars-application-args args '() env
;; 						   (lambda (fargs)
;; 						     (cont (set-union ff fargs)))))))


;; (define (free-vars-application-args args fset env cont)
;;   (cond
;;    ((null? args) (cont fset))
;;    (else (free-vars (car args) env (lambda (farg)
;; 				     (free-vars-application-args (cdr args)
;; 								 (set-union farg fset)
;; 								 env
;; 								 cont))))))

;;
;; 1 . washing
;; 2 . 


(define (fv-lambda? e)
  (and (pair? e)
       (eq? (car e) 'lambda)))


(define (fv-constant? e)
  (or (null? e)
      (number? e)
      (boolean? e)
      (string? e)))

(define (fv-bound? e b)  
  (and (symbol? e)
       (member e b)))


;; e : expression
;; b : bound variable list
;; k continuation

;; if [e] is a symbol then if its bound , no change , else add it to free vars
;; if [e] is not a symbol and not a pair , ignored other containers such as vectors , then its 
(define (fv e b f k)
  (cond   
   ((symbol? e)
    (if (fv-bound? e b)
	(k f)
	(k (set-union (list e) f))))
   ((fv-constant? e) (k f))
   ((fv-lambda? e) (fv-lambda e b f k))
   ((pair? e) (fv-app e b f k))
   (else
    (error "dont know how to process this " e b f k))))



;; simple lambda lists
;; (lambda (a b c) ... )
(define (fv-lambda e b f k)
  (let ((bindings (car (cdr e)))
	(body (cdr (cdr e))))
    (fv-body body (set-union bindings b) f k)))




(define (fv-body e b f k)
  (cond
   ((null? e) (k f))
   (else
    (fv (car e) b f (lambda (f2)
		      (fv-body (cdr e) b f2 k))))))

   

(define (fv-app e b f k)
  (fv-body e b f k))
		  

(define (freevars exp)
  (fv exp '() '() (lambda (x) x)))


;; x is free
(define (test1)
  (freevars '(lambda () x)))

;; no free vars
(define (test2)
  (freevars '(lambda (x) x)))


;; + y are free in expression  
(define (test3)
  (freevars '(lambda (x) (+ x y))))

(define (test4)
  (freevars '(define ktak
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
  (freevars '(lambda ()
	       (lambda (x) x)
	       (lambda (y) y)
	       (lambda (z)
		 (lambda (x)
		   (lambda (y)
		     z)
		   x)
		 z))))



