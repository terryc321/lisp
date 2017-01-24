
;; simple lisp interpreter in lisp

;; repl
;; read eval print loop

;; assume (read) is machine level primitive

(define first (lambda (xs) (car xs)))
(define second (lambda (xs) (car (cdr xs))))
(define third (lambda (xs) (car (cdr (cdr xs)))))


;; environemnt
(define m-env '((+ +)(- -)(* *)))


(define m-prompt (lambda (k)
		   (display "meta-prompt >")
		   (k #t)))

;; read expression
(define m-read (lambda (k) (k (read))))


;; evaluate an expression
(define m-eval (lambda (exp env k)
		 (cond
		   ((equal? exp 'bye) 'bye-bye)
		   ((number? exp) (k exp))
		   ((string? exp) (k exp))
		   ((vector? exp) (k exp))
		   ((boolean? exp) (k exp))
		   ((char? exp) (k exp))
		   ((pair? exp) (m-eval-pair exp env k))
		   (else (k exp)))))


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


;;
(define m-eval-pair (lambda (exp env k)
		      (cond
			((equal? (first exp) '+) (m-eval-add exp env k))
			((equal? (first exp) 'print) (m-eval-print exp env k))
			((equal? (first exp) 'newline) (m-eval-newline exp env k))
			((equal? (first exp) 'begin) (m-eval-begin exp env k))			
			((equal? (first exp) 'cons) (m-eval-cons exp env k))
			((equal? (first exp) 'car) (m-eval-car exp env k))
			((equal? (first exp) 'cdr) (m-eval-cdr exp env k))
			((equal? (first exp) 'quote) (m-eval-quote exp env k)) 
			((equal? (first exp) 'list) (m-eval-list exp env k))			
			(else (k exp)))))



;; show an expression
(define m-print (lambda (exp k)
		  (display "meta-result >")
		  (display exp)
		  (newline)
		  (k exp)))

;; repl
;; make a looping repl , pass it to itself , genius
(define m-repl
    (lambda (cont)
      (m-prompt (lambda (k)
		  (m-read (lambda (input)
			    (m-eval input m-env (lambda (result)
						  (m-print result (lambda (zzz) (m-repl m-repl)))))))))))











