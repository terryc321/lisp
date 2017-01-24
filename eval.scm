
;; simple lisp interpreter in lisp

;; repl
;; read eval print loop

;; assume (read) is machine level primitive
(define first (lambda (xs) (car xs)))
(define second (lambda (xs) (car (cdr xs))))
(define third (lambda (xs) (car (cdr (cdr xs)))))




(define m-flat-lookup
  (lambda (env key)
    (cond
     ((null? env) '())
     (else
      (let ((kv (car env)))
	(if (equal? (car kv) key)
	    kv
	    (m-flat-lookup (cdr env) key)))))))




(define m-lookup
  (lambda (env key)
    (cond
     ((null? env) '())
     (else
      (let ((kv (m-flat-lookup (car env) key)))
	(if (equal? kv '())	    
	    (m-lookup (cdr env) key)
	    kv))))))
  

(define m-set-environment
  (lambda (env key val)
    (let ((kv (m-lookup env key)))
      (if (equal? kv '())
	  #f
	  (set-car! (cdr kv) val)))))


(define m-prompt (lambda (k)
		   (display "meta-prompt >")
		   (k #t)))

;; read expression
(define m-read (lambda (k) (k (read))))


;; evaluate an expression
;; reader has to build
;;
;; quoted lists
;;
;; numbers
;; vectors
;; booleans
;; chars
;; strings
;;
;; if its a pair evaluate the operator
;; 
(define m-eval (lambda (exp env k)
		 (cond
		   ((equal? exp 'bye) 'bye-bye) ;; bye -- exits repl
		   ((number? exp) (k exp))
		   ((string? exp) (k exp))
		   ((vector? exp) (k exp))
		   ((boolean? exp) (k exp))
		   ((char? exp) (k exp))
		   ((symbol? exp)
		    (let ((kv (m-lookup env exp)))
		      (if (pair? kv)
			  (k (second kv))
			  (m-error exp env k (list "symbol not found :" exp " not found in env" env)))))
		   ((pair? exp)
		    (cond
		     ((symbol? (car exp))
		      ;(display "aha !! eval found given symbol as operator : ")
		      ;(display (car exp))
					;(newline)
		      (let ((symbol (car exp)))
			(let ((fobj (m-lookup env symbol)))
			  (begin
					;(display "fop = ") (display fop) (newline)
			    (if (closure? (second fobj))
				((second fobj) exp env k)
				(m-error exp env k
					 (list "err 89 symbol:" exp " in env: " env)))))))
		      (else (m-error exp env k
				     (list "err 91 ")))))
		   (else (m-error exp env k (list "err 92 :" exp " in env:" env))))))












(define m-error (lambda (exp env k obj)
		  (display "Error handler:")
		  (display obj)
		  (newline)
		  (display "Error exp:")
		  (display exp)
		  (newline)
		  (m-repl #f)))


(define m-handler (lambda (k)
		    (display "something went wrong")
		    (newline)
		    (m-repl m-repl)))



;; (+ n1 n2)
;; imagine + is a primitive unsafe operation
;; type check
;; if something is to go wrong need to pass it to current error handler
(define m-add (lambda (n1 n2 k)
		(cond
		  ((not (number? n1)) (m-handler k))
		  ((not (number? n2)) (m-handler k))
		  (else (k (+ n1 n2))))))


(define m-eval-add (lambda (exp env k)
		     (m-eval (second exp) env (lambda (a)
						(m-eval (third exp) env (lambda (b)
									  (m-add a b k)))))))


;; (set! a 5)
(define m-eval-set! (lambda (exp env k)
		      (m-eval (third exp) env (lambda (a)
						(m-set-environment env (second exp) a)
						(k a)))))



(define m-eval-print (lambda (exp env k)
		     (m-eval (second exp) env (lambda (a)
						(display a)
						(newline)
						(k a)))))

(define m-eval-newline (lambda (exp env k)
			 (newline)
			 (k exp)))


(define m-null? (lambda (exp k1 k2) (if (null? exp) (k1 #t) (k2 #f))))
(define m-last? (lambda (exp k1 k2) (if (null? (cdr exp)) (k1 #t) (k2 #f))))


(define m-eval-quote
    (lambda (exp env k)
      (k (second exp))))


;; strips begin from expression
;; provides a dummy return value
;; (begin) -> dummy
(define m-eval-begin (lambda (exp env k)
		       (m-eval-begin-loop (cdr exp) env k #:dummy)))

;; feed last value through until no more values to evaluate
(define m-eval-begin-loop (lambda (exp env k v)
			    (m-null? exp
				     (lambda (k2) (k v))
				     (lambda (k1)
				       (m-last? exp
						(lambda (k2) (m-eval (car exp) env k))
						(lambda (k3) (m-eval (car exp) env (lambda (k4)
										     (m-eval-begin-loop
										      (cdr exp)
										      env
										      k
										      k4)))))))))


(define m-reverse-helper
    (lambda (xs k ys)
      (cond
	((null? xs) (k ys))
	(else (m-reverse-helper (cdr xs) k (cons (car xs) ys))))))


(define m-reverse
    (lambda (xs k)
      (m-reverse-helper xs k '())))
		      


(define m-eval-list (lambda (exp env k)
		       (m-eval-list-loop (cdr exp) env k '())))

;; feed last value through until no more values to evaluate
;; we need to reverse them also at the end 
(define m-eval-list-loop (lambda (exp env k v)
			   (m-null? exp
				    (lambda (k2) (m-reverse v k))
				    (lambda (k1)
				      (m-eval (car exp) env (lambda (a)
							      (m-eval-list-loop (cdr exp)
										env
										k
										(cons a v))))))))
;; cons is a primitive machine level operation
(define m-eval-cons (lambda (exp env k)
		      (m-eval (second exp) env (lambda (a)
						 (m-eval (third exp) env (lambda (b)
									   (k (cons a b))))))))


;; car is a primitive machine level operation
(define m-eval-car (lambda (exp env k)
		     (m-eval (second exp) env (lambda (a)
						(k (car a))))))

;; cdr 
(define m-eval-cdr (lambda (exp env k)
		      (m-eval (second exp) env (lambda (a)
						 (k (cdr a))))))


;; (* n1 n1)
(define m-square (lambda (n1 k) (k (* n1 n1))))


;; catch
;; throw
(define m-eval-catch (lambda (exp env k) #f ))

;; (throw 'label something )
;;            evaluate the something
;;             then pass that result to handler set up by the catch
;;                                 if there is one , if not cry loudly.
(define m-eval-throw (lambda (exp env k) #f))




;; show an expression
(define m-print (lambda (exp k)
		  (display "meta-result >")
		  (display exp)
		  (newline)
		  (k exp)))

;; the environment
(define m-environment (list
		       (list
			(list 'a 5)
			(list 'b 6)
			(list 'c 7)
			(list 'foo 1)
			(list 'bar 2)
			(list 'baz 3)
			(list '+ m-eval-add)
			(list 'print m-eval-print)
			(list 'newline m-eval-newline)
			(list 'begin m-eval-begin)
			(list 'cons m-eval-cons)
			(list 'car m-eval-car)
			(list 'cdr m-eval-cdr)
			(list 'quote m-eval-quote)
			(list 'set! m-eval-set!)
			(list 'list m-eval-list))))






;; repl
;; make a looping repl , pass it to itself , genius
(define m-repl
    (lambda (cont)
      (m-prompt (lambda (k)
		  (m-read (lambda (input)
			    (m-eval input m-environment (lambda (result)
							  (m-print result m-repl)))))))))















