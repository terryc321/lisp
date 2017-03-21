



;; repl.scm
;; attempt understand meta circular interpreter with debugging support , maybe reflective procedures

;; each meta circular level gets its own environment , initialized to the global environment
;; everything is going through meta-apply



;;
;; Eval functions
;;
(define (base-eval exp env cont) 
  (cond ((number? exp)		 (meta-apply cont exp))
        ((boolean? exp)		 (meta-apply cont exp))
        ((string? exp)		 (meta-apply cont exp))

        ((symbol? exp)		 (meta-apply 'eval-var exp env cont))
        ((eq? (car exp) 'quote)  (meta-apply 'eval-quote exp env cont))
        ((eq? (car exp) 'if)	 (meta-apply 'eval-if exp env cont))
        ((eq? (car exp) 'cond)	 (meta-apply 'eval-cond (cdr exp) env cont))
        ((eq? (car exp) 'define) (meta-apply 'eval-define exp env cont))
        ((eq? (car exp) 'set!)	 (meta-apply 'eval-set! exp env cont))
        ((eq? (car exp) 'lambda) (meta-apply 'eval-lambda exp env cont))
        ((eq? (car exp) 'begin)  (meta-apply 'eval-begin (cdr exp) env cont))
        ((eq? (car exp) 'let)	 (meta-apply 'eval-let
                                             (car (cdr exp)) (cdr (cdr exp)) env cont))
        ((eq? (car exp) 'let*)	 (meta-apply 'eval-let*
                                             (car (cdr exp)) (cdr (cdr exp)) env cont))
        ((eq? (car exp) 'letrec) (meta-apply 'eval-letrec
                                             (car (cdr exp)) (cdr (cdr exp)) env cont))

        ((eq? (car exp) 'EM)	 (meta-apply 'eval-EM exp env cont))
        ((eq? (car exp) 'exec-at-metalevel)
         (meta-apply 'eval-EM exp env cont))
        ((eq? (car exp) 'primitive-EM)
         (meta-apply 'eval-primitive-EM exp env cont))
        ((eq? (car exp) 'exit)	 (meta-apply 'eval-exit exp env cont))
        ((eq? (car exp) 'load)	 (meta-apply 'eval-load exp env cont))
        ((eq? (car exp) 'and)	 (meta-apply 'eval-and (cdr exp) env cont))
        ((eq? (car exp) 'or)	 (meta-apply 'eval-or (cdr exp) env cont))
        ((eq? (car exp) 'delay)	 (meta-apply 'eval-delay exp env cont))
        ((eq? (car exp) 'cons-stream)  (meta-apply 'eval-cons-stream exp env cont))
        (else (meta-apply 'eval-application exp env cont))))

;; evaluates all arguments , then apply first to rest 
(define (eval-application exp env cont)
  (meta-apply 'eval-list exp env
	      (lambda (l) (meta-apply 'base-apply (car l) (cdr l) env cont))))


;; lookup variable , if found in environment , pass result to continuation
;; otherwise my-error unbound variable
(define (eval-var exp env cont)
  (let ((pair (get exp env)))
    (if (pair? pair)
	(meta-apply cont (cdr pair))
	(meta-apply 'my-error
		    (list 'eval-var: 'unbound 'variable: exp) env cont))))


;; pass 1nd argument unevaluated to continuation
(define (eval-quote exp env cont)
  (meta-apply cont (car (cdr exp))))



;; if the predicate is true , evaluate the then part , otherwise
;; if no ELSE part , pass result #f to continuation
;; otherwise do the ELSE part
(define (eval-if exp env cont)
  (let ((pred-part (car (cdr exp)))
	(then-part (car (cdr (cdr exp))))
	(else-part (cdr (cdr (cdr exp)))))
    (meta-apply 'base-eval pred-part env
                (lambda (p)
                  (cond (p (meta-apply 'base-eval then-part env cont))
                        ((null? else-part) (meta-apply cont #f))
                        (else
                         (meta-apply 'base-eval (car else-part) env cont)))))))


;; if no clauses pass it the empty list
;; if ELSE clause then treat clauses as sequence using eval-begin
(define (eval-cond clauses env cont)
  (cond ((null? clauses) (meta-apply cont '()))
	((eq? (car (car clauses)) 'else)
	 (meta-apply 'eval-begin (cdr (car clauses)) env cont))
	(else
	 (meta-apply 'base-eval
		     (car (car clauses))
		     env
		     (lambda (pred)
		       (if pred
			   (meta-apply 'eval-begin (cdr (car clauses))
				       env cont)
			   (meta-apply 'eval-cond (cdr clauses)
				       env cont)))))))


;; make some definitions
;; (define (f x) ... )
;;   car (cdr exp ) = (f x)
;;  car (car (cdr exp))) = f
;;
;; (define (f x) ...) = (define f (lambda (x ...) ...)
;; evaluate the lambda expression , set that to be named
;; pass the continuation the name of the variable
;; ie => f 
(define (eval-define exp env cont)
  (if (pair? (car (cdr exp)))
      (let ((var (car (car (cdr exp))))
	    (body (cons 'lambda
			(cons (cdr (car (cdr exp)))
			      (cdr (cdr exp))))))
	(meta-apply 'base-eval body env
		    (lambda (data)
		      (define-value var data env)
		      (meta-apply cont var))))
      ;; otherwise 
      (let ((var (car (cdr exp)))
	    (body (car (cdr (cdr exp)))))
	(meta-apply 'base-eval body env
		    (lambda (data)
		      (define-value var data env)
		      (meta-apply cont var))))))



;; evaluate 2nd expression , set value of 1st expression in environment
;; if not found then call my-error 
(define (eval-set! exp env cont)
  (let ((var (car (cdr exp)))
	(body (car (cdr (cdr exp)))))
    (meta-apply 'base-eval body env
		(lambda (data)
		  (let ((pair (get var env)))
		    (if (pair? pair)
			(begin (set-value! var data env)
			       (meta-apply cont var))
			(meta-apply 'my-error
				    (list 'eval-set!: 'unbound 'variable var)
				    env cont)))))))


;; the actual tag
(define lambda-tag (cons 'lambda 'tag))


;; says pulls apart lambda expression , and stores environment and tags it with the
;; lambda tag
;; passes this to the continuation
(define (eval-lambda exp env cont)
  (let ((lambda-body (cdr (cdr exp)))
	(lambda-params (car (cdr exp))))
    (meta-apply cont (list lambda-tag lambda-params lambda-body env))))


;; basic sequencing
;; if its the last expression then base-eval that ,
;; otherwise have continuation evaluate the rest of the sequence
(define (eval-begin body env cont)
  (define (eval-begin-local body)
    (if (null? (cdr body))
	(meta-apply 'base-eval (car body) env cont)
	(meta-apply 'base-eval (car body) env
		    (lambda (x) (eval-begin-local (cdr body))))))
  (if (null? body)
      (meta-apply 'my-error '(eval-begin: null body) env cont)
      (eval-begin-local body)))


;; all local extending environment , standard stuff , nothing to see here , moving on.
(define (eval-let pairs body env cont)
  (let ((params (map car pairs))
	(args (map (lambda (x) (car (cdr x))) pairs)))
    (meta-apply 'eval-list args env
		(lambda (operand)
		  (meta-apply 'eval-begin body
			      (extend env params operand)
			      cont)))))

(define (eval-let* pairs body env cont)
  (if (null? pairs)
      (meta-apply 'eval-begin body env cont)
      (meta-apply 'base-eval (car (cdr (car pairs))) env (lambda (arg)
		  (meta-apply 'eval-let* (cdr pairs) body
			      (extend env (list (car (car pairs))) (list arg))
			      cont)))))

(define (eval-letrec pairs body env cont)
  (define (set-value-list! params operand env)
    (if (null? params)
	#f
	(begin (set-value! (car params) (car operand) env)
	       (set-value-list! (cdr params) (cdr operand) env))))
  (let ((params (map car pairs))
	(args (map (lambda (x) (car (cdr x))) pairs)))
    (let ((letrec-env (extend env params params)))
      (meta-apply 'eval-list args letrec-env
		  (lambda (operand)
		    (set-value-list! params operand letrec-env)
		    (meta-apply 'eval-begin body letrec-env cont))))))



;; evaluate list items from left to right
;; consing up the result after all rest of the list has been evaluated
(define (eval-list exp env cont)
  (if (null? exp)
      (meta-apply cont '())
      (meta-apply 'base-eval (car exp) env
		  (lambda (val1)
		    (meta-apply 'eval-list (cdr exp) env
				(lambda (val2)
				  (meta-apply cont (cons val1 val2))))))))




;; exit allows us to go to a higher metalevel
;; do this by evaluating argument to exit
;; then deliberately call an error , which bump into higher meta-level
(define (eval-exit exp env cont)
  (meta-apply 'base-eval (car (cdr exp)) env
	      (lambda (x)
                (meta-apply 'my-error x env cont))))





;; if map - simple map f x , single arguments
;; if scheme-apply -
;; some stuff to do with lazy streams , force delay and sleep.
(define (base-apply operator operand env cont)
  (cond ((procedure? operator)
	 (cond ((eq? operator map)                
		(meta-apply 'eval-map
			    (car operand) (car (cdr operand)) env cont))
	       ((eq? operator scheme-apply)
		(meta-apply 'base-apply
			    (car operand) (car (cdr operand)) env cont))
	       ((eq? operator force)
		(let ((arg (car operand)))
		  (if (and (pair? arg)
			   (eq? (car arg) delay-tag))
		      (let ((promise-body (car (cdr arg)))
			    (promise-env (car (cdr (cdr arg))))
			    (pair (cdr (cdr arg))))
			(if (pair? (cdr pair))
			    (meta-apply cont (car (cdr pair)))
			    (meta-apply 'base-eval promise-body promise-env
					(lambda (ans)
					  (set-cdr! pair (list ans))
					  (meta-apply cont ans)))))
		      (meta-apply cont arg))))
	       ((pair? (member operator primitive-procedures))
		(meta-apply cont (scheme-apply operator operand)))
	       (else ;; called when going down a level.
		(lambda (Mcont)
		  ((scheme-apply operator operand)
		   (cons-stream (list (get-global-env env) cont) Mcont))))))
        ;; 
	((and (pair? operator)
	      (eq? (car operator) lambda-tag))
	 (let ((lambda-params        (car (cdr operator)))
	       (lambda-body     (car (cdr (cdr operator))))
	       (lambda-env (car (cdr (cdr (cdr operator))))))
           ;; are the operands and parameters matched up 
	   (if (can-receive? lambda-params operand)
	       (meta-apply 'eval-begin
			   lambda-body
			   (extend lambda-env lambda-params operand)
			   cont)
	       (meta-apply 'my-error
			   (list 'base-apply: 'Wrong 'number 'of 'arguments:
				 operand 'to: lambda-params)
			   env cont))))
	(else
	 (meta-apply 'my-error (list 'Not 'a 'function: operator) env cont))))























;; exec-at-metalevel 
(define (eval-EM exp env cont)
  (lambda (Mcont)
    (let ((meta-env (car (head Mcont)))
	  (meta-cont (car (cdr (head Mcont))))
	  (meta-Mcont (tail Mcont)))
      ((meta-apply 'base-eval
		   (car (cdr exp))
		   meta-env
		   (lambda (ans) (lambda (Mcont2)
		     ((meta-apply cont ans)
		      (cons-stream (head Mcont) Mcont2)))))
	 meta-Mcont))))



(define (eval-primitive-EM exp env cont)
  (meta-apply 'base-eval (car (cdr exp)) env
	      (lambda (body)
                (meta-apply 'eval-EM (list 'EM body) env cont))))

















  
