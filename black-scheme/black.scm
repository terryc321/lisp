;; black system

;; some infinite stream stuff for the lazy meta continuation stack
;; ------------------------------------------------------------------------------
;; delay and force
; R5RS syntactic abstraction:
(define-syntax delay 
  (syntax-rules ()
    ((delay expr)
     (lambda ()
        expr))))

(define (force promise) (promise))

(define head car)
(define (tail stream) (force (cdr stream)))
(define empty-stream? null?)
(define the-empty-stream '())

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))
;; ------------------------------------------------------------------------------

(define (primitive-write arg)
  (if (and (pair? arg)
	   (eq? (car arg) lambda-tag))
      (let ((lambda-params        (car (cdr arg)))
	    (lambda-body     (car (cdr (cdr arg))))
	    (lambda-env (car (cdr (cdr (cdr arg))))))
	(write (cons 'lambda (cons lambda-params lambda-body))))
      (write arg)))



(define (init-cont env level turn cont)
  (cont (lambda (answer)
	  (write level) (write '-) (write turn) (display ": ")
	  (primitive-write answer) (newline)
	  (write level) (write '-) (write (+ turn 1)) (display "> ")
	  (base-eval (read) env
		     (lambda (ans) (init-cont env level (+ turn 1)
					      (lambda (cont) (cont ans))))))))
(define (run env level answer)
  (init-cont env level 0 (lambda (cont) (cont answer))))




;; all of section 5 excluding secion 5.1 including appendix 


;; section 5.2
;; Cont = Value -> Mcont -> Answer
;; functions : Exp * Env * Cont -> Mcont -> Answer
;; meta-apply : Symbol or Proc * List of Values -> Mcont -> Answer
(define (base-eval exp env cont)
  (cond ((number? exp)           (meta-apply cont exp))
	((boolean? exp)          (meta-apply cont exp))
	((string? exp)           (meta-apply cont exp))
	((symbol? exp)           (meta-apply 'eval-var exp env cont))
	((eq? (car exp) 'quote)  (meta-apply 'eval-quote exp env cont))
	((eq? (car exp) 'if)     (meta-apply 'eval-if exp env cont))
	((eq? (car exp) 'set!)   (meta-apply 'eval-set! exp env cont))
	((eq? (car exp) 'lambda) (meta-apply 'eval-lambda exp env cont))
	((eq? (car exp) 'begin)  (meta-apply 'eval-begin exp env cont))
	((eq? (car exp) 'exec-at-metalevel)
	 (meta-apply 'eval-EM exp env cont))
	((eq? (car exp) 'exit)   (meta-apply 'eval-exit exp env cont))
	(else (meta-apply 'eval-application exp env cont))))

(define (eval-var exp env cont)
  (let ((pair (get exp env)))
    (if (pair? pair)
	(meta-apply cont (cdr pair))
	(meta-apply 'my-error
		     (list 'eval-var: 'unbound 'variable: exp) env cont))))

(define (eval-quote exp env cont) (meta-apply cont (car (cdr exp))))

(define (eval-if exp env cont)
  (meta-apply 'base-eval (car (cdr exp)) env
	       (lambda (pred)
		 (if pred (meta-apply 'base-eval (car (cdr (cdr exp)))
				       env cont)
		     (meta-apply 'base-eval (car (cdr (cdr (cdr exp))))
				  env cont)))))

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

(define lambda-tag (cons 'lambda 'tag))

(define (eval-lambda exp env cont)
  (let ((lambda-body (cdr (cdr exp)))
	(lambda-params (car (cdr exp))))
    (meta-apply cont (list lambda-tag lambda-params lambda-body env))))


;; eval-EM    : Exp * Env * Cont -> Mcont -> Answer
;; meta-apply : Symbol or Proc * List of Values -> Mcont -> Answer
(define (eval-EM exp env cont)
  (lambda (Mcont)
    (let ((meta-env (car (head Mcont)))
	  (meta-cont (car (cdr (head Mcont))))
	  (meta-Mcont (tail Mcont)))
      ((meta-apply 'base-eval (car (cdr exp))
		    meta-env
		    (lambda (ans)
		      (lambda (Mcont2)
			((meta-apply cont ans)
			 (cons-stream (head Mcont) Mcont2)))))
       meta-Mcont))))


;; base-apply : Proc * List of Values * Env * Cont -> Mcont -> Answer
(define (base-apply operator operand env cont)
  (cond ((procedure? operator)
	 (cond ((eq? operator map)
		(meta-apply 'eval-map
			     (car operand) (car (cdr operand)) env cont))
	       ((eq? operator scheme-apply)
		(meta-apply 'base-apply
			     (car operand) (car (cdr operand)) env cont))
	       ((pair? (memq operator primitive-procedures))
		(meta-apply cont (scheme-apply operator operand)))
	       (else ; evaluator functions <=========================== (A)
		(lambda (Mcont)
		  ((scheme-apply operator operand)
		   (cons-stream (list (get-global-env env) cont)
				Mcont))))))
	((and (pair? operator)
	      (eq? (car operator) lambda-tag))
	 (let ((lambda-params        (car (cdr operator)))
	       (lambda-body     (car (cdr (cdr operator))))
	       (lambda-env (car (cdr (cdr (cdr operator))))))
	   (meta-apply 'eval-begin-body
			lambda-body
			(extend lambda-env lambda-params operand)
			cont)))
	(else (meta-apply 'my-error
			   (list 'Not 'a 'function: operator) env cont))))




;; meta-apply : Symbol or Proc * List of Values -> Mcont -> Answer
(define (meta-apply proc-name-or-cont . operand)
  (lambda (Mcont)
    (let* ((meta-env (car (head Mcont)))
	   (meta-cont (car (cdr (head Mcont))))
	   (meta-Mcont (tail Mcont))
	   (operator (if (symbol? proc-name-or-cont)
			 (cdr (get proc-name-or-cont meta-env))
			 proc-name-or-cont)))
      (cond ((procedure? operator)
	     (if (pair? (memq operator primitive-procedures))
		 ((meta-apply 'base-apply operator operand
			       meta-env meta-cont)
		  meta-Mcont)
		 ((scheme-apply operator operand) ; evaluator functions
		  Mcont)))
	    (else
	     ((meta-apply 'base-apply operator operand meta-env meta-cont)
	      meta-Mcont))))))



(define old-env 0)
(define old-cont 0)

;; my-error : Exp * Env * Cont -> Mcont -> Answer
(define (my-error exp env cont)
  (lambda (Mcont)
    (let ((meta-env (car (head Mcont)))
	  (meta-cont (car (cdr (head Mcont))))
	  (meta-Mcont (tail Mcont)))
      (set-value! 'old-env env meta-env)
      (set-value! 'old-cont cont meta-env)
      ((meta-apply meta-cont exp) meta-Mcont))))


(define (black)
  (let* ((base-Mcont (init-Mcont 0 (copy init-env)))
	 (env (car (head base-Mcont)))
	 (cont (car (cdr (head base-Mcont))))
	 (Mcont (tail base-Mcont)))
    ((cont 'start) Mcont)))

(define (init-Mcont level env-below)
  (let ((env (copy init-env)))
    (cons-stream (list env (meta-init-cont env level env-below))
		 (init-Mcont (+ level 1) env))))



(define (meta-init-cont env level env-below)
  (define-value 'init-env env-below env) ; share-env
  (display "New level loaded.") (newline)
  (lambda (result) (meta-apply 'run env level result)))

(define scheme-apply apply)

(define (primitive-procedure? . operand)
  (let ((arg (car operand)))
    (or (procedure? arg)
	(and (pair? arg)
	     (eq? (car arg) lambda-tag)))))




;; appendix A.2
(define empty-env '(()))
(define (make-pairs params args)
  (cond ((null? params) '())
	((symbol? params) (list (cons params args)))
	(else
	 (cons (cons (car params) (car args))
	       (make-pairs (cdr params) (cdr args))))))
(define (extend env params args)
  (cons (make-pairs params args) env))
(define (get var env)
  (if (null? env)
      '()
       (let ((pair (assq var (car env))))
	 (if (pair? pair)
	     pair
	     (get var (cdr env))))))
(define (set-value! var value env)
  (let ((pair (get var env)))
    (if (pair? pair)
	(set-cdr! pair value)
	#f)))
(define (define-value var value env)
  (let ((pair (assq var (car env))))
    (if (pair? pair)
	(set-cdr! pair value)
	(set-car! env (cons (cons var value) (car env))))))
(define (copy env)
  (define (copy-local env)
    (if (null? env)
	'()
	 (cons (cons (car (car env))
		     (cdr (car env)))
	       (copy-local (cdr env)))))
  (if (null? env)
      '()
       (cons (copy-local (car env))
	     (copy (cdr env)))))
(define (get-global-env env)
  (define (get-global-env-local env)
    (if (null? (cdr env))
	env
	(get-global-env-local (cdr env))))
  (if (null? env)
      env
      (get-global-env-local env)))


;; Appendix A.3
;; evaluator functions : Exp * Env * Cont -> Mcont -> Answer
(define (eval-begin exp env cont)
  (meta-apply 'eval-begin-body (cdr exp) env cont))
(define (eval-begin-body body env cont)
  (define (eval-begin-local body)
    (if (null? (cdr body))
	(meta-apply 'base-eval (car body) env cont)
	(meta-apply 'base-eval (car body) env
		     (lambda (x) (eval-begin-local (cdr body))))))
  (if (null? body)
      (meta-apply 'my-error '(eval-begin-body: null body) env cont)
      (eval-begin-local body)))
(define (eval-exit exp env cont)
  (meta-apply 'base-eval (car (cdr exp)) env
	       (lambda (x) (meta-apply 'my-error x env cont))))
(define (eval-list exp env cont)
  (if (null? exp)
      (meta-apply cont '())
      (meta-apply 'base-eval (car exp) env
		   (lambda (val1)
		     (meta-apply 'eval-list (cdr exp) env
				  (lambda (val2)
				    (meta-apply cont (cons val1 val2))))))))
(define (eval-application exp env cont)
  (meta-apply 'eval-list exp env
	       (lambda (l) (meta-apply 'base-apply (car l) (cdr l) env cont))))
(define (eval-map fun lst env cont)
  (if (null? lst)
      (meta-apply cont '())
      (meta-apply 'base-apply fun (list (car lst)) env
		   (lambda (x) (meta-apply 'eval-map fun (cdr lst) env
					    (lambda (y) (meta-apply cont (cons x y))))))))
;; init-cont : Env * Number * Number * Cont -> Mcont -> Answer
(define (init-cont env level turn cont)
  (meta-apply cont
	      (lambda (answer)
		(write level) (write '-) (write turn) (display ": ")
		(primitive-write answer) (newline)
		(write level) (write '-) (write (+ turn 1)) (display "> ")
		(meta-apply 'base-eval (read) env
			     (lambda (ans)
			       (meta-apply 'init-cont env level (+ turn 1)
					    (lambda (cont) (meta-apply cont ans))))))))
;; run : Env * Number * Value -> Mcont -> Answer
(define (run env level answer)
  (meta-apply 'init-cont env level 0
	       (lambda (cont) (meta-apply cont answer))))

(define primitive-procedures #f)

(define init-env (list (list
			(cons 'car        car)        (cons 'cdr              cdr)
			(cons 'cons       cons)       (cons 'list             list)
			(cons 'pair?      pair?)      (cons 'null?            null?)
			(cons 'not        not)        (cons 'eq?              eq?)
			(cons 'eqv?       eqv?)       (cons 'equal?           equal?)
			(cons 'set-car!   set-car!)   (cons 'set-cdr!         set-cdr!)
			(cons 'append     append)     (cons 'newline          newline)
			(cons 'read       read)
			(cons 'write            primitive-write)
			(cons '+ +)       (cons '- -) (cons '* *)             (cons '/ /)
			(cons '= =)       (cons '> >) (cons '< <)
			(cons 'quotient   quotient)   (cons 'remainder        remainder)
			(cons 'number?    number?)    (cons 'symbol?          symbol?)
			(cons 'boolean?   boolean?)   (cons 'string?          string?)
			(cons 'memq       memq)       (cons 'length           length)
			(cons 'assq       assq)
			(cons 'procedure?       primitive-procedure?)
			(cons 'map        map)        (cons 'scheme-apply     scheme-apply)
			(cons 'empty-env  empty-env)  (cons 'make-pairs       make-pairs)
			(cons 'extend     extend)     (cons 'set-value!       set-value!)
			(cons 'get        get)        (cons 'define-value     define-value)
			(cons 'copy       copy)       (cons 'get-global-env   get-global-env)
			(cons 'base-eval  base-eval)  (cons 'eval-var         eval-var)
			(cons 'eval-quote eval-quote) (cons 'eval-if          eval-if)
			(cons 'lambda-tag lambda-tag) (cons 'eval-lambda      eval-lambda)
			(cons 'eval-begin eval-begin) (cons 'eval-begin-body  eval-begin-body)
			(cons 'eval-set!  eval-set!)  (cons 'eval-EM          eval-EM)
			(cons 'eval-exit  eval-exit)  (cons 'eval-application eval-application)
			(cons 'eval-list  eval-list)  (cons 'base-apply       base-apply)
			(cons 'my-error   my-error)   (cons 'eval-map         eval-map)
			(cons 'init-env   0)
					; to be filled later
			(cons 'init-cont  init-cont)  (cons 'run            run)
			(cons 'old-env    old-env)    (cons 'old-cont       old-cont)
			(cons 'primitive-procedures   primitive-procedures)
			)))

(define-value 'init-env (copy init-env) init-env)

(set! primitive-procedures
  (list car cdr cons list pair? null? not eq? eqv? equal? set-car! set-cdr!
	append newline read primitive-write + - * / = > < quotient remainder
	number? symbol? boolean? string? memq length assq primitive-procedure?
	map scheme-apply
	make-pairs extend get set-value! define-value copy get-global-env))





