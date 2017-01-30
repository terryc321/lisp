;; -*- coding: utf-8 -*- 
;;

(define (base-eval exp env cont)
  (cond ((number? exp)                      (cont exp))
	((boolean? exp)                     (cont exp))
	((string? exp)                      (cont exp))
	((symbol? exp)                      (eval-var exp env cont))
	((eq? (car exp) ’quote)             (eval-quote exp env cont))
	((eq? (car exp) ’if)                (eval-if exp env cont))
	((eq? (car exp) ’set!)              (eval-set! exp env cont))
	((eq? (car exp) ’lambda)            (eval-lambda exp env cont))
	((eq? (car exp) ’begin)             (eval-begin exp env cont))
	((eq? (car exp) ’exec-at-metalevel) (eval-EM exp env cont))
	((eq? (car exp) ’exit)              (eval-exit exp env cont))
	(else (eval-application exp env cont))))

(define (eval-var exp env cont)
  (let ((pair (get exp env)))
    (if (pair? pair)
	(cont (cdr pair))
	(my-error (list ’eval-var: ’unbound ’variable: exp) env cont))))

(define (eval-quote exp env cont) (cont (car (cdr exp))))

(define (eval-if exp env cont)
  (base-eval (car (cdr exp)) env
	     (lambda (pred)
	       (if pred (base-eval (car (cdr (cdr exp))) env cont)
		   (base-eval (car (cdr (cdr (cdr exp)))) env cont)))))

(define (eval-set! exp env cont)
  (let ((var (car (cdr exp)))
	(body (car (cdr (cdr exp)))))
    (base-eval body env
	       (lambda (data)
		 (let ((pair (get var env)))
		   (if (pair? pair)
		       (begin (set-value! var data env)
			      (cont var))
		       (my-error (list ’eval-set!: ’unbound ’variable var)
				 env cont)))))))

(define lambda-tag (cons ’lambda ’tag))

(define (eval-lambda exp env cont)
  (let ((lambda-body (cdr (cdr exp)))
	(lambda-params (car (cdr exp))))
    (cont (list lambda-tag lambda-params lambda-body env))))

(define (eval-begin exp env cont)
  (eval-begin-body (cdr exp) env cont))

(define (eval-begin-body body env cont)
  (define (eval-begin-local body)
    (if (null? (cdr body))
	(base-eval (car body) env cont)
	(base-eval (car body) env (lambda (x) (eval-begin-local (cdr body))))))
  (if (null? body)
      (my-error ’(eval-begin-body: null body) env cont)
      (eval-begin-local body)))

(define (eval-exit exp env cont)
  (base-eval (car (cdr exp)) env (lambda (x) (my-error x env cont))))

(define (eval-list exp env cont)
  (if (null? exp)
      (cont ’())
      (base-eval (car exp) env
		 (lambda (val1)
		   (eval-list (cdr exp) env
			      (lambda (val2) (cont (cons val1 val2))))))))

(define (eval-application exp env cont)
  (eval-list exp env (lambda (l) (base-apply (car l) (cdr l) env cont))))

(define (eval-map fun lst env cont)
  (if (null? lst)
      (cont ’())
      (base-apply fun (list (car lst)) env
		  (lambda (x) (eval-map fun (cdr lst) env
					(lambda (y) (cont (cons x y))))))))


(define (eval-EM exp env cont)
  (cont (primitive-EM (car (cdr exp)))))

(define (base-apply operator operand env cont)
  (cond ((procedure? operator)
	 (cond ((eq? operator map)
		(eval-map (car operand) (car (cdr operand)) env cont))
	       ((eq? operator scheme-apply)
		(base-apply (car operand) (car (cdr operand)) env cont))
	       ((pair? (memq operator primitive-procedures))
		(cont (scheme-apply operator operand)))
	       (else ; evaluator functions
		(cont (scheme-apply operator operand)))))
	((and (pair? operator)
	      (eq? (car operator) lambda-tag))
	 (let ((lambda-params        (car (cdr operator)))
	       (lambda-body     (car (cdr (cdr operator))))
	       (lambda-env (car (cdr (cdr (cdr operator))))))
	   (eval-begin-body lambda-body
			    (extend lambda-env lambda-params operand)
			    cont)))
	(else
	 (my-error (list ’Not ’a ’function: operator) env cont))))

(define old-env 0)

(define old-cont 0)

(define (my-error exp env cont)
  (set! old-env env)
  (set! old-cont cont)
  exp)


(define (init-cont env level turn cont)
  (cont (lambda (answer)
	  (write level) (write ’-) (write turn) (display ": ")
	  (primitive-write answer) (newline)
	  (write level) (write ’-) (write (+ turn 1)) (display "> ")
	  (base-eval (read) env
		     (lambda (ans) (init-cont env level (+ turn 1)
					      (lambda (cont) (cont ans))))))))

(define (run env level answer)
  (init-cont env level 0 (lambda (cont) (cont answer))))

(define init-env (list (list
			(cons ’car        car)        (cons ’cdr              cdr)
			(cons ’cons       cons)       (cons ’list             list)
			(cons ’pair?      pair?)      (cons ’null?            null?)
			(cons ’not        not)        (cons ’eq?              eq?)
			(cons ’eqv?       eqv?)       (cons ’equal?           equal?)
			(cons ’set-car!   set-car!)   (cons ’set-cdr!         set-cdr!)
			(cons ’append     append)     (cons ’newline          newline)
			(cons ’read       read)
			(cons ’write            primitive-write)
			(cons ’+ +)       (cons ’- -) (cons ’* *)             (cons ’/ /)
			(cons ’= =)       (cons ’> >) (cons ’< <)
			(cons ’quotient   quotient)   (cons ’remainder        remainder)
			(cons ’number?    number?)    (cons ’symbol?          symbol?)
			(cons ’boolean?   boolean?)   (cons ’string?          string?)
			(cons ’memq       memq)       (cons ’length           length)
			(cons ’assq       assq)
			(cons ’procedure? primitive-procedure?)
			(cons ’map        map)        (cons ’scheme-apply     scheme-apply)
			(cons ’empty-env  empty-env)  (cons ’make-pairs       make-pairs)
			(cons ’extend     extend)     (cons ’set-value!       set-value!)
			(cons ’get        get)        (cons ’define-value     define-value)
			(cons ’copy       copy)       (cons ’get-global-env   get-global-env)
			(cons ’base-eval  base-eval)  (cons ’eval-var         eval-var)
			(cons ’eval-quote eval-quote) (cons ’eval-if          eval-if)
			(cons ’lambda-tag lambda-tag) (cons ’eval-lambda      eval-lambda)
			(cons ’eval-begin eval-begin) (cons ’eval-begin-body  eval-begin-body)
			(cons ’eval-set!  eval-set!)  (cons ’eval-EM          eval-EM)
			(cons ’eval-exit  eval-exit)  (cons ’eval-application eval-application)
			(cons ’eval-list  eval-list)  (cons ’base-apply       base-apply)
			(cons ’my-error   my-error)   (cons ’eval-map         eval-map)
			(cons ’init-env   0)
					; to be filled later
			(cons ’init-cont  init-cont)  (cons ’run            run)
			(cons ’old-env    old-env)    (cons ’old-cont       old-cont)
			(cons ’primitive-procedures   primitive-procedures)
			)))

(define-value ’init-env (copy init-env) init-env)




