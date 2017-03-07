
;; cps repl

;;(letrec ((fib (lambda (n) (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2))))))) (fib 10))
;;(letrec ((odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))) (even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))) (odd? 33))
;; (letrec ((odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))) (even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))) (even? 33))
;; (define map (lambda (f xs) (if (null? xs) xs (cons (f (car xs)) (map f (cdr xs))))))


;; include a mechanism for indirection into the evaluator , we no longer need to
;; concern ourselves with mutating list structures
(define indirect-tag #f)
(define indirect-tagged? #f)
(define indirect-untag #f)

(let ((special (gensym "indirect")))
  (set! indirect-tag (lambda () special))
  (set! indirect-tagged? (lambda (exp) (and (pair? exp)
				       (eq? (car exp) special))))
  (set! indirect-untag (lambda (exp) (cdr exp))))


;; normal repl -- assumes everything ok.
;; debugger repl -- when something goes wrong.
(define (read-k k)
  (k (read)))


;; everything is true
(define (base-eval exp env cont)
  (cond
   ((number? exp) (cont exp))
   ((boolean? exp) (cont exp))
   ((string? exp) (cont exp))
   ((vector? exp) (cont exp))
   ((symbol? exp) (lookup-symbol exp env cont))
   ((indirect-tagged? exp) ;; follow the indirection
    (base-eval (indirect-untag exp) env cont))
   ((tagged-lambda? exp) (cont exp))
   ((pair? exp) (base-eval-pair exp env cont))
   (else
    (error "base-eval unknown expr type" exp env cont))))



(define (tagged-lambda? exp)
  (and (pair? exp)
       (lambda-tag? (car exp))))


(define (base-eval-pair exp env cont)
  (let ((op (car exp)))
    (cond
     ;; load
     ((eq? op 'load) (eval-load exp env cont))
     ;;
     ((eq? op 'newline) (eval-newline exp env cont))
     ;; define
     ((eq? op 'define) (eval-define exp env cont))
     ;; set!
     ((eq? op 'set!) (eval-set! exp env cont))
     ;; begin
     ((eq? op 'begin) (eval-begin exp env cont))
     ;; cond
     ((eq? op 'cond) (eval-cond exp env cont))     
     ;; gensym
     ((eq? op 'gensym) (eval-gensym exp env cont))
     ;; callcc
     ((eq? op 'callcc) (eval-callcc exp env cont))     
     ;; case     
     ;; quoted data
     ((eq? op 'quote) (eval-quote exp env cont))
     ;; selection
     ((eq? op 'if) (eval-if exp env cont))
     ;; lambda abstraction 
     ((eq? op 'lambda) (eval-lambda exp env cont))
     ;; let = could be just lambda application     
     ((eq? op 'let) (eval-let exp env cont))
     ;; let* = macro let
     ((eq? op 'let*) (eval-let-star exp env cont))
     ;; letrec = macro let + set!
     ((eq? op 'letrec) (eval-letrec exp env cont))     
     ;; some predicates
     ((eq? op 'null?) (eval-null? exp env cont))
     ((eq? op 'pair?) (eval-pair? exp env cont))
     ;; some list stuff 
     ((eq? op 'cons) (eval-cons exp env cont))     
     ((eq? op 'car) (eval-car exp env cont))
     ((eq? op 'cdr) (eval-cdr exp env cont))
     ;; math -- limited to 2 arg format for now 
     ((eq? op '+) (eval-add exp env cont))
     ((eq? op '-) (eval-sub exp env cont))
     ((eq? op '*) (eval-mul exp env cont))
     ((eq? op '/) (eval-div exp env cont))
     ((eq? op 'eqv?) (eval-eqv? exp env cont))
     ((eq? op 'eq?) (eval-eq? exp env cont))
     ((eq? op '>) (eval-greater-than exp env cont))
     ((eq? op '<) (eval-less-than exp env cont))
     ((eq? op '=) (eval-num-eq exp env cont))
     ((eq? op 'list) (eval-list exp env cont))
     ;; i o 
     ((eq? op 'read) (eval-read exp env cont))
     ((eq? op 'display) (eval-display exp env cont))     
     ;; otherwise
     (else (eval-application exp env cont)))))


;;
(define (eval-newline exp env cont)
  (newline)
  (cont #f))

;; cond = macro on ifs 
;; (cond (x ..)(y ..)(else ...))
;; (if x ... (if y ... ....))
(define (eval-cond exp env cont)
 (base-eval (eval-cond-helper (cdr exp))
 	     env
 	     cont))




;; 
(define (eval-cond-helper exp)
  (format "eval cond helper : ~a ~%" exp)
  (cond
   ((null? exp) #f)
   ((eq? (car (car exp)) 'else) `(begin ,@(cdr (car exp))))
   (else `(if ,(car (car exp)) (begin ,@(cdr (car exp)))
	      ,(eval-cond-helper (cdr exp))))))


;;
(define (eval-gensym exp env cont)
  (cond
   ((null? (cdr exp))
    (cont (gensym)))
   (else (cont (gensym (car (cdr exp)))))))




;;
;;
;;
(define (eval-load exp env cont)
  (let ((filename (car (cdr exp))))
    (let ((result
	   (with-input-from-file filename
	     (lambda ()
	       (letrec ((f (lambda (val)
			     (let ((expr (read)))
			       (if (eof-object? expr)
				   val
				   (begin
				     (format #t "read from file ~a ~%" expr)
				     (base-eval expr env (lambda (val)
							   (f val)))))))))
		 (f #f))))))
      (cont result))))



    


(define (eval-null? exp env cont)
  (let ((the-part (car (cdr exp))))
    (base-eval the-part
	       env
	       (lambda (v) (cont (null? v))))))


(define (eval-pair? exp env cont)
  (let ((the-part (car (cdr exp))))
    (base-eval the-part
	       env
	       (lambda (v) (cont (pair? v))))))


;; (callcc (lambda (k) ....))
(define (eval-callcc exp env cont)
  (let ((the-lambda (car (cdr exp))))
    (base-eval `(,the-lambda ',cont)
	       env
	       cont)))



;; letrec
;; (letrec ((x ...)(y ...)(z ...)) ... body ...)
;; (let ((x #f)(y #f)(z #f)) (define x ...)(define y ...)(define z ...) ... body ...)
(define (eval-letrec exp env cont)
  (let ((pairs (car (cdr exp)))
	(body (cdr (cdr exp))))
    (let ((vars (map car pairs)))
      (let ((vars-false (map (lambda (x) (list x #f)) vars))
	    ;; could use (cons 'define  
	    (defines (map (lambda (x) (cons 'set! x)) pairs)))
	(base-eval `(let ,vars-false ,@defines (begin ,@body))
		   env
		   cont)))))


;; let* construct is just a macro
;; (let* ((x ...)(y ...)) ... body...)
;; (let ((x ..))
;;    (let ((y ...))
;;        ... body ... 
(define (eval-let-star exp env cont)
  (let ((pairs (car (cdr exp)))
	(body (cdr (cdr exp))))
    ;;(cont (eval-let-star-helper pairs body))))
    (base-eval (eval-let-star-helper pairs body)
	       env
	       cont)))   
;;
(define (eval-let-star-helper pairs body)
  (if (null? pairs)
      ;; result is then just body
      `(begin ,@body)
      ;; more pairs to process
      `(let (,(car pairs)) ,(eval-let-star-helper (cdr pairs) body))))



;; let construct
(define (eval-let exp env cont)
  (let ((pairs (car (cdr exp)))
	(body (cdr (cdr exp))))
  (eval-let-helper pairs body env cont)))

(define (eval-let-helper pairs body env cont)	 
  (let ((params (map car pairs))
	(args (map (lambda (x) (car (cdr x))) pairs)))
    ;;(format #t "params = ~a ~%" params)
    ;;(format #t "args = ~a ~%" args)    
    (eval-list-sequence args env
		(lambda (operands)
		  (eval-begin-sequence
		   body
		   (extend-env params operands env)
		   #f
		   cont)))))



(define (base-apply operator operands env cont)
  (cond
   ((tagged-lambda? operator)
    (let ((lambda-params        (car (cdr operator)))
	  (lambda-body     (car (cdr (cdr operator))))
	  (lambda-env (car (cdr (cdr (cdr operator))))))
      (eval-begin-sequence
       lambda-body
       (extend-env lambda-params operands lambda-env)
       #f
       cont)))
   ;; only primitive procedures are introduced using callcc
   ;; remarkably this is all that is needed for callcc to work
   ((procedure? operator) 
    (operator operands))
   (else
    (error "not a function: " operator env cont))))



;; put new list onto front of
;; but not destructively , this is a cons operation , so it isnt like define or set!
(define (extend-env lambda-params operands lambda-env)
  (cons (map (lambda (k v) (cons k v))
	     lambda-params
	     operands)
	lambda-env))


(define (eval-application exp env cont)
  (eval-list-sequence exp env
	     (lambda (l) (base-apply (car l) (cdr l) env cont))))

(define (eval-list-sequence exp env cont)
  (if (null? exp)
      (cont '())
      (base-eval (car exp) env
		  (lambda (val1)
		    (eval-list-sequence (cdr exp) env
				(lambda (val2)
				  (cont (cons val1 val2))))))))


(define (eval-list exp env cont)
  (let ((exprs-part (cdr exp)))
    (eval-list-sequence exprs-part env cont)))



;; lookup
;; environment is a list of assoc-lists
;; ...
;;   ((a . 1)(b . 2)(c . 3))
;;   ((x . 1)(y . 2)(z . 3))
;; ...
(define (lookup-symbol exp env cont)
  (cond
   ((null? env)
    (error "lookup : no such variable " exp env cont))
   ((assoc exp (car env))
    (cont (cdr (assoc exp (car env)))))
   (else (lookup-symbol exp (cdr env) cont))))


   
;; tease apart two types of define
(define (eval-define exp env cont)
  (if (pair? (car (cdr exp)))
      ;; (define (f x) ...)
      (eval-define-fun exp env cont)
      ;; (define f ...)
      (eval-define-var exp env cont)))


;; (define (f x) ...)
;; (define f (lambda (x) ....))
(define (eval-define-fun exp env cont)  
  (let ((var (car (car (cdr exp))))
	(args (cdr (car (cdr exp))))
	(body (cdr (cdr exp))))
    (base-eval `(define ,var (lambda ,args ,@body))
	       env
	       cont)))



;; (define f ...)
(define (eval-define-var exp env cont)
  (let ((var (car (cdr exp)))
	(body (car (cdr (cdr exp)))))
    (base-eval body env
	       (lambda (data)
		 (env-define! var data env cont)))))


;; (set! f ...)
(define (eval-set! exp env cont)
  (let ((var (car (cdr exp)))
	(body (car (cdr (cdr exp)))))
    (base-eval body
	       env
	       (lambda (data)
		 (env-set! var data env cont)))))





;; if value exists , then clobber it
;; if value does not exist , then destructively mutate front of the list 
;; may require toplevel global environment is initially a cons
(define (env-define! var val env cont)
  ;; provide 2 continuations
  ;; 1 success - mutation applied
  ;; 2 failed  - make toplevel manual mutation
  ;;  on failed assuming environment has atleast a list in the CAR position
  ;; this allows us to mutate it
  (env-define-helper var val env cont (lambda (ignore)
					 (set-car! env (cons (cons var val)
							     (car env)))
					 (cont val))))

(define (env-define-helper var val env cont fail)
  (cond
   ;; no more levels to traverse -- symbol not found -- fail
   ((null? env) (fail #t))
   ;; if symbol is in first assoc list then mutate it -- and succeed
   ((assoc var (car env))
    (assoc-set! (car env) var val)
    (cont val))
   ;; otherwise keep looking in other environment levels
   (else (env-define-helper var val (cdr env) cont fail))))


(define (env-set! var val env cont)
  (env-set-helper var val env cont (lambda (ignore)
				     (error "set! no binding for variable " var val env cont))))

(define (env-set-helper var val env cont fail)
  (cond
   ;; no more levels to traverse -- symbol not found -- fail
   ((null? env) (fail #t))
   ;; if symbol is in first assoc list then mutate it -- and succeed
   ((assoc var (car env))
    (assoc-set! (car env) var val)
    (cont val))
   ;; otherwise keep looking in other environment levels
   (else (env-set-helper var val (cdr env) cont fail))))


;; (define (env-has-var var val env top)
;;   (cond
;;    ((null? env) #f)
;;    (or (env-has-var-assoc var val (car env))
;;        (env-has-var var val (cdr env) top))))

;; (define (env-has-var-assoc var val kvs)
;;   (cond
;;    ((null? kvs) #f)
;;    ((eq? 

(define (eval-begin exp env cont)
  (let ((body-part (cdr exp))
	(dummy-value #f))
    (eval-begin-sequence body-part env dummy-value cont)))


(define (eval-begin-sequence body env val cont)
  (if
   (null? body)
   (cont val)
   (base-eval (car body) env (lambda (new-val)
			       (eval-begin-sequence (cdr body) env new-val cont)))))



(define (eval-read exp env cont)
  (cont (read)))

(define (eval-display exp env cont)
  (let ((thing-part (car (cdr exp))))
    (base-eval thing-part env
	       (lambda (v1)
		 (display v1)
		 (cont v1)))))


(define (eval-greater-than exp env cont)
  (let ((a-part (car (cdr exp)))
	(b-part (car (cdr (cdr exp)))))
    (base-eval a-part env
	       (lambda (v1)
		 (base-eval b-part env
			    (lambda (v2)
			      (cont (> v1 v2))))))))

(define (eval-less-than exp env cont)
  (let ((a-part (car (cdr exp)))
	(b-part (car (cdr (cdr exp)))))
    (base-eval a-part env
	       (lambda (v1)
		 (base-eval b-part env
			    (lambda (v2)
			      (cont (< v1 v2))))))))

(define (eval-num-eq exp env cont)
  (let ((a-part (car (cdr exp)))
	(b-part (car (cdr (cdr exp)))))
    (base-eval a-part env
	       (lambda (v1)
		 (base-eval b-part env
			    (lambda (v2)
			      (cont (= v1 v2))))))))


(define (eval-add exp env cont)
  (let ((a-part (car (cdr exp)))
	(b-part (car (cdr (cdr exp)))))
    (base-eval a-part env
	       (lambda (v1)
		 (base-eval b-part env
			    (lambda (v2)
			      (cont (+ v1 v2))))))))

(define (eval-sub exp env cont)
  (let ((a-part (car (cdr exp)))
	(b-part (car (cdr (cdr exp)))))
    (base-eval a-part env
	       (lambda (v1)
		 (base-eval b-part env
			    (lambda (v2)
			      (cont (- v1 v2))))))))

(define (eval-mul exp env cont)
  (let ((a-part (car (cdr exp)))
	(b-part (car (cdr (cdr exp)))))
    (base-eval a-part env
	       (lambda (v1)
		 (base-eval b-part env
			    (lambda (v2)
			      (cont (* v1 v2))))))))

(define (eval-div exp env cont)
  (let ((a-part (car (cdr exp)))
	(b-part (car (cdr (cdr exp)))))
    (base-eval a-part env
	       (lambda (v1)
		 (base-eval b-part env
			    (lambda (v2)
			      (cont (/ v1 v2))))))))

(define (eval-eq? exp env cont)
  (let ((a-part (car (cdr exp)))
	(b-part (car (cdr (cdr exp)))))
    (base-eval a-part env
	       (lambda (v1)
		 (base-eval b-part env
			    (lambda (v2)
			      (cont (eq? v1 v2))))))))

(define (eval-eqv? exp env cont)
  (let ((a-part (car (cdr exp)))
	(b-part (car (cdr (cdr exp)))))
    (base-eval a-part env
	       (lambda (v1)
		 (base-eval b-part env
			    (lambda (v2)
			      (cont (eqv? v1 v2))))))))




;; lambda with symbol as params
;; (lambda args ...)

;; lambda with list params
;; (lambda (x y z) ...)

;; dotted lambda 
;; (lambda (x y . z) ...) 
;; lambda tag is gensym interned so it can be faked at the repl
;; if we didnt want to be faked at repl , make it uninterned symbol
(define eval-lambda #f)
(define lambda-tag? #f)
(let ((lambda-tag (gensym "lambda-tag")))
  (set! eval-lambda
    (lambda (exp env cont)
      (let ((lambda-body (cdr (cdr exp)))
	    (lambda-params (car (cdr exp))))
	(cont (list lambda-tag lambda-params lambda-body env)))))
  (set! lambda-tag? (lambda (tag) (eq? tag lambda-tag))))

(define (eval-quote exp env cont)
  (cont (car (cdr exp))))

(define (eval-if exp env cont)
  (let ((pred-part (car (cdr exp)))
	(then-part (car (cdr (cdr exp))))
	(else-part (cdr (cdr (cdr exp)))))
    (base-eval pred-part env
	       (lambda (p)
		(cond (p (base-eval then-part env cont))
		      ((null? else-part) (cont #f))
		      (else
		       (base-eval (car else-part) env cont)))))))

(define (eval-cons exp env cont)
  (let ((car-part (car (cdr exp)))
	(cdr-part (car (cdr (cdr exp)))))
    (base-eval car-part env
	       (lambda (v1)
		 (base-eval cdr-part env
			    (lambda (v2)
			      (cont (cons v1 v2))))))))

(define (eval-car exp env cont)
  (let ((cons-part (car (cdr exp))))
    (base-eval cons-part env
	       (lambda (v1)
		 (cont (car v1))))))

(define (eval-cdr exp env cont)
  (let ((cons-part (car (cdr exp))))
    (base-eval cons-part env
	       (lambda (v1)
		 (cont (cdr v1))))))


;; bare ass environment
(define bare-ass-environment (cons '() '()))

(define environment bare-ass-environment)

;; repl is just a loop , that is all it does -- for now.
(define (repl2)
  (begin
    (newline)
    (display "prompt2] =>")
    (read-k (lambda (exp)
	      (cond
	       ((eq? exp 'q) #t)
	       ;; evaluate the expression in the toplevel environment
	       (else (base-eval exp environment
			     (lambda (val)
			       (display "value2]  => ")
			       (display val)
			       (newline)
			       (repl2)))))))))




