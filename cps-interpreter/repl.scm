


(define *traced-routines* '())



;;;  guile specific stuff
;;(define gensym gensym)

;;; mit specific stuff
(define gensym generate-uninterned-symbol)


;;; (use-modules (ice-9 pretty-print))
;;; (load "/home/terry/lisp/cps-interpreter/core/synclo.scm")



(define *debug-level* 0)




;;----------------- fexpr ---------------------------
(define fexpr-tag #f)
(define fexpr-tagged? #f)
(define fexpr-untag #f)
(define make-fexpr #f)

(let ((special (gensym "fexpr")))
  (set! make-fexpr (lambda (params body env) (list special params body env)))
  (set! fexpr-tag (lambda () special))
  (set! fexpr-tagged? (lambda (exp) (and (pair? exp)
				       (eq? (car exp) special))))
  (set! fexpr-untag (lambda (exp) (cdr exp))))





;;----------------- lambda ---------------------------
(define lambda-tag #f)
(define lambda-tagged? #f)
(define lambda-untag #f)
(define make-lambda #f)

(let ((special (gensym "lambda")))
  (set! make-lambda (lambda (params body env) (list special params body env)))
  (set! lambda-tag (lambda () special))
  (set! lambda-tagged? (lambda (exp) (and (pair? exp)
				       (eq? (car exp) special))))
  (set! lambda-untag (lambda (exp) (cdr exp))))


;; ---------------- indirection datatype ----------------------
;; include a mechanism for indirectionion into the evaluator , we no longer need to
;; concern ourselves with mutating list structures
(define indirection-tag #f)
(define indirection-tagged? #f)
(define indirection-untag #f)

(let ((special (gensym "indirection")))
  (set! indirection-tag (lambda () special))
  (set! indirection-tagged? (lambda (exp) (and (pair? exp)
				       (eq? (car exp) special))))
  (set! indirection-untag (lambda (exp) (cdr exp))))

;; ---------------- environment datatype ----------------------
(define environment-tag #f)
(define environment-tagged? #f)
(define environment-untag #f)
(define make-environment #f)

(let ((special (gensym "environment")))
  (set! make-environment (lambda (op) (cons special op)))
  (set! environment-tag (lambda () special))
  (set! environment-tagged? (lambda (exp) (and (pair? exp)
				       (eq? (car exp) special))))
  (set! environment-untag (lambda (exp) (cdr exp))))

;; ---------------- continuation datatype ----------------------
(define continuation-tag #f)
(define continuation-tagged? #f)
(define continuation-untag #f)
(define make-continuation #f)

(let ((special (gensym "continuation")))
  (set! make-continuation (lambda (op) (cons special op)))
  (set! continuation-tag (lambda () special))
  (set! continuation-tagged? (lambda (exp) (and (pair? exp)
				       (eq? (car exp) special))))
  (set! continuation-untag (lambda (exp) (cdr exp))))


;; ---------------- cps-primitive datatype ----------------------
(define cps-primitive-tag #f)
(define cps-primitive-tagged? #f)
(define cps-primitive-untag #f)
(define cps-primitive #f)

(let ((special (gensym "prim-cps")))
  (set! cps-primitive (lambda (op) (cons special op)))
  (set! cps-primitive-tag (lambda () special))
  (set! cps-primitive-tagged? (lambda (exp) (and (pair? exp)
				       (eq? (car exp) special))))
  (set! cps-primitive-untag (lambda (exp) (cdr exp))))




;;-----------------------------------------------------------------
;;(define pprint (lambda (x) "...yep yep ..."))

(define pprint
  (lambda (x)
    (cond
     ((cps-primitive-tagged? x) (display "<cps-primitive>"))
     ((continuation-tagged? x) (display "<continuation>"))
     ((lambda-tagged? x)
      (display "<...lambda...> "))
      ;;(display (car (cdr x)))      
      ;;(display (car (cdr (cdr x)))))
     ((fexpr-tagged? x) (display "<...flambda...>"))
     ((environment-tagged? x) (display "<environment>"))
     ((symbol? x) (display x))
     ((boolean? x) (display x))
     ((number? x) (display x))
     ((null? x) (display "()"))
     ((string? x) (write x))
     ;; cps - function
     ((cps-function? x) (display "<cps-function>"))
     
     ((procedure? x) (display "<underlying-scheme-procedure>"))     
     ((pair? x)
      (pprint-list x))
     (else (error "dont know how to display this " x )))))

(define pprint-list
  (lambda (the-list)
    (display "(")
    (letrec ((pprint-contents (lambda (xs)
				(cond
				 ;; nothing more to do
				 ((null? xs) #f)
				 ;; last element -- and we done
				 ((null? (cdr xs))
				  (pprint (car xs)))
				 ;; dotted pair -- and we are done
				 ((not (pair? (cdr xs)))
				  (pprint (car xs))
				  (display " . ")
				  (pprint (cdr xs)))
				 (else
				  (pprint (car xs))
				  (display " ")
				  (pprint-contents (cdr xs)))))))
      (pprint-contents the-list))
    (display ")")))


;;-----------------------------------------------------------------
(define (breakpoint exp env cont)
  (newline)
  (display "BREAKPOINT [??")
  (display "] :> ")
  (let ((form (read)))
    (if (eof-object? form)
	;; finish here , this will terminate cps loop
	#f
	(if (and (pair? form)
		 (eq? (car form) 'continue))
	    ;; continue with some value
	    (base-eval (car (cdr form))
		       env
		       (lambda (v1)
			 (cont v1)))
	    ;; evaluate the form , but disgard result, loop
	    (base-eval form
		       env
		       (lambda (v1)
			 (newline)
			 (display v1)			 
			 (breakpoint #f env cont)))))))


(define (debug exp env cont)
  (newline)
  (display "DEBUG [")
  (display level)
  (display "] :> ")
  (let ((form (read)))
    (if (eof-object? form)
	;; finish here , this will terminate cps loop
	#f
	(if (and (pair? form)
		 (eq? (car form) 'continue))
	    ;; continue with some value
	    (base-eval (car (cdr form))
		       env
		       (lambda (v1)
			 (cont v1)))
	    ;; evaluate the form , but disgard result, loop
	    (base-eval form
		       env
		       (lambda (v1)
			 (newline)
			 (display v1)			 
			 (debug #f env cont)))))))



(define (debugger message exp env cont)
  (newline)
  (display "DEBUGGER ::")
  (display message)
  (newline)  
  (debug exp env cont))


;; normal repl -- assumes everything ok.
;; debugger repl -- when something goes wrong.
(define (read-k k)
  (let ((form (read)))
    (if (eof-object? form)
	;; finish here
	#f
	;; otherwise pass form to continuation
	(k form))))




;; everything is true
(define (base-eval exp env cont)
  ;; is there a hook attached to this expression ?
  ;; is there a before hook ?
  ;; is there a during hook ?
  ;; is there an after hook ?
  (cond
   
   ;; numbers
   ((number? exp) (cont exp))
   
   ;; booleans
   ((boolean? exp) (cont exp))
   
   ;; strings
   ((string? exp) (cont exp))
   
   ;; array
   ((vector? exp) (cont exp))
   
   ;; underlying scheme procedure huh ?
   ((procedure? exp) (cont exp))

   ;; cps-function are themselves
   ((cps-function? exp) (cont exp))
   
   ;; symbols
   ((symbol? exp) (lookup-symbol exp env cont))
   
   ;; continuations are just like lambd-as
   ((continuation-tagged? exp) (cont exp))
   
   ;; underlying procedure --> huh ??
   ;;((procedure? exp) (cont exp))
   ;; cps-primitives are themselves -- like a lamb-da
   ((cps-primitive-tagged? exp)   (cont exp))
   
   ;; ;; indirections -- not used yet
   ;; ((indirection-tagged? exp) 
   ;;  (base-eval (indirection-untag exp) env cont))
   ((lambda-tagged? exp) (cont exp))
   
   ;; flambdas
   ((fexpr-tagged? exp) (cont exp))
   
   ;; user defined data types are self evaluating in general.
   ((environment-tagged? exp) (cont exp))
   
   ;; (primitive-apply ... ...)
   ;;((and (pair? exp) (= (length exp) 3) (eq? (car exp) 'primitive-apply))
   ;; (eval-primitive-apply exp env cont))

   ;; (apply ... ...)
   ((and (pair? exp) (>= (length exp) 3) (eq? (car exp) 'apply))
    (eval-apply exp env cont))
   
   ;; (begin ... ...)
   ((and (pair? exp) (eq? (car exp) 'begin))
    (eval-begin exp env cont))
   
   ;; set! X Y
   ((and (pair? exp)
	 (= (length exp) 3)
	 (eq? (car exp) 'set!))
    (eval-set! exp env cont))

   
   ;; quote X
   ((and (pair? exp) (= (length exp) 2) (eq? (car exp) 'quote))
    (cont (car (cdr exp))))
   
   ;; if x y
   ;; if x y z
   ((and (pair? exp) (or (= (length exp) 3)
			 (= (length exp) 4))
	 (eq? (car exp) 'if))
    (eval-if exp env cont))
   
   ;; (lambda ... ...)
   ((and (pair? exp) (>= (length exp) 3) (eq? (car exp) 'lambda))
    (eval-lambda exp env cont))

   ;; (let ... ...)
   ((and (pair? exp) (>= (length exp) 3) (eq? (car exp) 'let))
    (eval-let exp env cont))
   ;; let* in terms let
   ;; letrec in terms of let
   
   ;; breakpoint
   ((and (pair? exp) (eq? (car exp) 'breakpoint))
    (breakpoint exp env cont))         

   ;; trace
   ((and (pair? exp) (eq? (car exp) 'trace))
    (eval-trace exp env cont))         
   
   ;;       
   ((pair? exp) (base-eval-pair exp env cont))
   (else
    (error "base-eval unknown expr type" exp env cont))))


(define (base-eval-pair exp env cont)
  (let ((op (car exp)))
    ;; there was dispatch routines here
    ;; but we moved it to environment
    (cond
     (else (eval-application exp env cont)))))

(define eval-fexpr
    (lambda (exp env cont)
      (let ((fexpr-body (cdr (cdr exp)))
	    (fexpr-params (car (cdr exp))))
	(cont (make-fexpr fexpr-params fexpr-body env)))))



(define eval-lambda 
    (lambda (exp env cont)
      (let ((lambda-body (cdr (cdr exp)))
	    (lambda-params (car (cdr exp))))
	(cont (make-lambda lambda-params lambda-body env)))))


;; ---------------------- map --------------------------------------------
;; surprisingly difficult to get this right , really.
;; map f  xs
;; map f2 xs ys
;; map f3 xs ys zs
;; (define (eval-map exp env cont)
;;   (let ((fun-part (car (cdr exp)))
;; 	(vals-part  (cdr (cdr exp))))
;;     (base-eval fun-part
;;     	       env
;;     	       (lambda (fun)
;;     		 (eval-list-sequence vals-part
;;     				     env
;;     				     (lambda (vals)
;; 				       (base-eval 
;; 					(cons 'list (map-lots fun vals))
;; 					env
;; 					cont)))))))

;; (define (map-lots f vals)
;;   (cond
;;    ((null? vals) '())
;;    ;; bottom of vals ie ( () () () () )
;;    ((null? (car vals)) '())
;;    (else (cons (cons f (map car vals))
;; 	       (map-lots f (map cdr vals))))))


;; -------------------------------------------------------------------------


;; ;;         (format "fun part = ~a ~%" fun-part)
;; ;;     (format "vals-part = ~a ~%" vals-part)
;; ;;     (cont vals-part)))
;; ;; (my-map-f fun-part vals-part))))

;; 					;; (list vals
;; 					;; 	   (cons (cons fun (my-cars vals))
;; 					;; 		 (my-cdrs vals))))))))))

;; ;;(my-map fun vals))))))))

;; ;; '((1 2 3)(4 5 6)(7 8 9))
;; ;; => '(1 4 7)
;; (define (my-cars vals)
;;   (cond
;;    ((null? vals) '())
;;    (else (cons (car (car vals))
;; 	       (my-cars (cdr vals))))))


;; ;; '((1 2 3)(4 5 6)(7 8 9))
;; ;; => '((2 3)(4 5)(8 9))
;; (define (my-cdrs vals)
;;   (cond
;;    ((null? vals) '())
;;    (else (cons (cdr (car vals))
;; 	       (my-cdrs (cdr vals))))))


;; (define (my-map fun vals)
;;   (cond
;;    ((null? vals) '())
;;    (else 
;;     (cons (cons fun (car vals))
;; 	  (my-map fun (cdr vals))))))


;; (define (my-map-f fun vals)
;;   (cond
;;    ((null? vals) '())
;;    (else 
;;     (cons (cons fun (my-cars vals))
;; 	  (my-map-f fun (my-cdrs vals))))))



;; set-car!
(define (eval-set-car! exp env cont)
  (let ((cons-part (car (cdr exp)))
	(val-part  (car (cdr (cdr exp)))))
    (base-eval cons-part
	       env
	       (lambda (v1)
		 (base-eval val-part
			    env
			    (lambda (v2)
			      (cont (set-car! v1 v2))))))))


;; set-cdr!
(define (eval-set-cdr! exp env cont)
  (let ((cons-part (car (cdr exp)))
	(val-part  (car (cdr (cdr exp)))))
    (base-eval cons-part
	       env
	       (lambda (v1)
		 (base-eval val-part
			    env
			    (lambda (v2)
			      (cont (set-cdr! v1 v2))))))))







;; assuming alist is well formed .
(define (my-assoc key alist)
  (cond
   ((null? alist) #f)
   ((eq? key (car (car alist)))
    (car alist))
   (else (my-assoc key (cdr alist)))))


;; assoc
(define (eval-assoc exp env cont)
  (let ((key (car (cdr exp)))
	(alist (car (cdr (cdr exp)))))
    (base-eval key
	       env
	       (lambda (v1)
		 (base-eval alist
			    env
			    (lambda (v2)
			      (cont (my-assoc v1 v2))))))))


;; ;; and
;; ;; (and) = #t
;; ;; (and x) = x
;; ;; (and x y) = (if x (if y #f))
;; ;; (and x y z) = (if x x (if y y (if z z #f)))
;; (define (eval-and exp env cont)
;;   (cond
;;    ;; (and) = #t
;;    ((null? (cdr exp)) (cont #t))
;;    ;; (and x) = x 
;;    ((null? (cdr (cdr exp)))
;;     (base-eval (car (cdr exp))
;; 	       env
;; 	       cont))
;;    ;; (and x y)
;;    ;; (and x y z)
;;    ;; (and x y z p) ...
;;    (else
;;     (base-eval (eval-and-helper (cdr exp))
;; 	       env
;; 	       cont))))

;; (define (eval-and-helper exp)
;;   ;;(format "eval and helper : ~a ~%" exp)
;;   (cond
;;    ;; ( z ) 
;;    ((null? (cdr exp))
;;     (let ((gs (gensym)))
;;       `(let ((,gs ,(car exp)))
;; 	 (if ,gs ,gs #f))))
;;    ;; ( x ... )
;;    (else
;;     (let ((gs (gensym)))
;;       `(let ((,gs ,(car exp)))
;; 	 (if ,gs ,(eval-and-helper (cdr exp)) #f))))))





;; ;; or
;; ;; and
;; ;; (and) = #t
;; ;; (and x) = x
;; ;; (and x y) = (if x (if y #f))
;; ;; (and x y z) = (if x x (if y y (if z z #f)))
;; (define (eval-or exp env cont)
;;   (cond
;;    ;; (or) = #f
;;    ((null? (cdr exp))
;;     (cont #f))
;;    ;; (or x) = x 
;;    ((null? (cdr (cdr exp)))
;;     (base-eval (car (cdr exp))
;; 	       env
;; 	       cont))
;;    ;; (or x y)
;;    ;; (or x y z)
;;    ;; (or x y z p) ...
;;    (else 
;;     (base-eval (eval-or-helper (cdr exp))
;; 	       env
;; 	       cont))))


;; (define (eval-or-helper exp)
;;   ;;(format "eval or helper : ~a ~%" exp)
;;   (cond
;;    ;; ( z ) 
;;    ((null? (cdr exp))
;;     (let ((gs (gensym)))
;;       `(let ((,gs ,(car exp)))
;; 	 (if ,gs ,gs #f))))
;;    ;; ( x ... )
;;    (else
;;     (let ((gs (gensym)))
;;       `(let ((,gs ,(car exp)))
;; 	 (if ,gs ,gs ,(eval-or-helper (cdr exp))))))))

;; ;; not

(define (eval-trace exp env cont)
  (let ((the-routines (cdr exp)))
    (cond
     ((null? the-routines)
      (set! *traced-routines* '())
      (cont *traced-routines*))
     (else 
      (map (lambda (routine)
	     (cond
	      ((symbol? routine) (set! *traced-routines* (cons routine *traced-routines*)))
	      (#t #f)))
	   the-routines)
      (cont *traced-routines*)))))




(define (eval-symbol? exp env cont)
  (let ((the-part (car (cdr exp))))
    (base-eval the-part
	       env
	       (lambda (v) (cont (symbol? v))))))


(define (eval-vector? exp env cont)
  (let ((the-part (car (cdr exp))))
    (base-eval the-part
	       env
	       (lambda (v) (cont (vector? v))))))



(define (eval-boolean? exp env cont)
  (let ((the-part (car (cdr exp))))
    (base-eval the-part
	       env
	       (lambda (v) (cont (boolean? v))))))





(define (eval-string? exp env cont)
  (let ((the-part (car (cdr exp))))
    (base-eval the-part
	       env
	       (lambda (v) (cont (string? v))))))




(define (eval-number? exp env cont)
  (let ((the-part (car (cdr exp))))
    (base-eval the-part
	       env
	       (lambda (v) (cont (number? v))))))

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


(define (eval-cond-helper exp)
  ;;(format "eval cond helper : ~a ~%" exp)
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
				     ;;(format #t "~% *> : ~a ~%" expr)
				     (newline)
				     (display "::> ")
				     (pprint expr)
				     (let ((expanded-expr (macro-expand expr)))
				       (newline)
				       (base-eval expanded-expr
						  env
						  (lambda (val)
						    ;;(format #t "~% => : ~a ~%" val)
						    (newline)
						    (display "==> ")
						    (pprint val)
						    (f val))))))))))
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
;; this is only place where we build continuations manually for now
(define (eval-callcc exp env cont)
  (let ((the-lambda (car (cdr exp))))
    (base-eval `(,the-lambda ',(make-continuation cont))
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
;;
;;
(define (eval-let-star-helper pairs body)
  (if (null? pairs)
      ;; result is then just body
      `(begin ,@body)
      ;; more pairs to process
      `(let (,(car pairs)) ,(eval-let-star-helper (cdr pairs) body))))


;;;
;;;
;;;
;;; let construct
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
		   (extend-environment params operands env)
		   #f
		   cont)))))




;; dont actually use the environment in base apply
(define (base-apply operator operands env cont)
  (cond
   
   ;;  fexprs
   ((fexpr-tagged? operator)
    (let ((fexpr-params        (car (cdr operator)))
	  (fexpr-body     (car (cdr (cdr operator))))
	  (fexpr-env (car (cdr (cdr (cdr operator))))))
      (eval-begin-sequence
       fexpr-body
       (extend-environment fexpr-params operands fexpr-env)
       #f
       cont)))   
   
   ;;  lambdas
   ((lambda-tagged? operator)
    (let ((lambda-params        (car (cdr operator)))
	  (lambda-body     (car (cdr (cdr operator))))
	  (lambda-env (car (cdr (cdr (cdr operator))))))
      (eval-begin-sequence
       lambda-body
       (extend-environment lambda-params operands lambda-env)
       #f
       cont)))   
   ;; only primitive procedures are introduced using callcc
   ;; remarkably this is all that is needed for callcc to work

   ((continuation-tagged? operator)
    (let ((location (continuation-untag operator)))
      ;;(format #t "scheme continuation : ~a ~%" operator)
      ;;(format #t "scheme location     : ~a ~%" location)      
      ;;(format #t "scheme operands     : ~a ~%" operands)
      ;;(format #t "passing continuation a result of : ~a ~%" (car operands))    
      (location (car operands))))

   ((cps-function? operator)
    (newline)
    (display "cps-function: operator = ")
    (display operator)
    (newline)
    (display "cps-function: operands = ")
    (display operands)
    (newline)    
    ((cps-function-obj operator) operands env cont))
   
   ((procedure? operator)
    (newline)
    (display " operator = ")
    (display operator)
    (newline)
    (display " operands = ")
    (display operands)
    (newline)
    ;;(format #t "underlying scheme procedure : ~a ~%" operator)
    ;;(format #t "           scheme operands  : ~a ~%" operands)    
    (cont (apply operator operands)))   
   (else
    (error "not a function: " operator env cont))))







;; extend-environment
;; 
;; put new list onto front of
;; but not destructively , this is a cons operation , so it isnt like define or set!
;;
;; lambda params may be dotted pair
;; 
;;  (x y z )   no slurping , all matched exactly
;;  (x y  . z )  so z slurps up any and all other parameters
;;  xyz  so xyz slurps up everything into one argument
(define (extend-environment params operands env)
  (cond
   ;; no more params to process
   ((null? params) env)
   ;; one symbol - slurp remaining operands if any
   ((symbol? params) (cons params
			   (cons operands env)))
   ;; last parameter - not slurpy
   ((null? (cdr params))
    (cons (car params)
	  (cons (car operands) env)))
   (else
    ;; match up one parameter with one operand
    (cons (car params)
	  (cons
	   (car operands)
	   (extend-environment (cdr params) (cdr operands) env))))))









(define (eval-application exp env cont)
  (let ((proc (car exp))
	(args (cdr exp)))
    (base-eval proc
	       env
	       ;; evaluated procedure eproc
	       (lambda (eproc) 
		 (cond
		  ;; fexpr -- pass to base apply without evaluating the args
		  ((fexpr-tagged? eproc)
		   (base-apply eproc args env cont))
		  
		  ;; cps-primitive 
		  ((cps-primitive-tagged? eproc)
		   
		   ;;(format #t "applying cps-primitive ~a ~%" eproc)
		   ;;(format #t "cps-args : ~a ~%" args)
		   ;;(format #t "cps-cont : ~a ~%" cont)
		   
		   ;; hand control off to cps-primitive
		   ;; cps-primitive expects its name to be in operator position
		   ;; e.g (map f ....)
		   ;; map cps-primitive expects map in operator position
		   (let ((dummy-exp (cons proc args)))
		     ((cps-primitive-untag eproc) dummy-exp env cont)))
		  ;; otherwise 
		  (else
		   ;; evaluate all the arguments in the current environment
		   (eval-list-sequence args env
				       ;; evaluated args eargs
				       (lambda (eargs)
					 ;;(format #t " primitive [eproc] ~a " eproc)
					 ;;(format #t " : [args] ~a ~%" eargs)
					 (cond
					  ((lambda-tagged? eproc)
					   ;; user defined procedure from e.g fib.scm

					   ;; hook associated with this particular lambda list
					   ;; might want lambda id , in addition to lambda tag ,
					   ;; so we can uniquely identify that lambda expression
					   ;; globally
					   ;; use eq? for now					   
					   (let ((lambda-params        (car (cdr eproc)))
						 (lambda-body     (car (cdr (cdr eproc))))
						 (lambda-env (car (cdr (cdr (cdr eproc))))))
					     (eval-begin-sequence
					      lambda-body
					      (extend-environment lambda-params eargs lambda-env)
					      #f
					      (lambda (result)
						(cond
						 ((member proc *traced-routines*)
						  (newline)
						  (display (cons proc eargs))
						  (display "==>")
						  (display result)
						  (cont result))
						 (else
						  (cont result)))))))
					  (else
					   (base-apply eproc eargs env cont)))))))))))


					 ;; (cont (cons eproc eargs))
					 ;; ))))))))
					 ;; ;;(base-apply proc eargs env cont)))))))))



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
;; flat environment list = (a 1 b 2 c 3)
;; sym = car env
;; val = car cdr env
;;    next symbol = cdr cdr env
(define (lookup-symbol exp env cont)
  (cond
   ((null? env)
    (error "lookup : no such variable " exp env cont))
   ((eq? exp (car env))
    (cont (car (cdr env))))
   (else (lookup-symbol exp (cdr (cdr env)) cont))))



   
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


;; mutate environment
;; but we do not want to create new memory address as the cons
;; 




;; if value exists , then clobber it
;; if value does not exist , then destructively mutate front of the list 
;; may require toplevel global environment is initially a cons
(define (env-define! var val env cont)
  ;; provide 2 continuations
  ;; 1 success - mutation applied
  ;; 2 failed  - make toplevel manual mutation
  ;;  on failed assuming environment has atleast a list in the CAR position
  ;; this allows us to mutate it
  (env-define-helper var
		     val
		     env
		     cont
		     (lambda (ignore)
		       (let ((old-car (car env))
			     (old-cdr (cdr env)))
			 (set-car! env var)
			 (set-cdr! env (cons val
					     (cons old-car old-cdr)))
			 (cont val)))))


(define (env-define-helper var val env cont fail)
  (cond
   ;; no more levels to traverse -- symbol not found -- fail
   ((null? env) (fail #t))
   ;; if symbol is in first 
   ((eq? var (car env))
    (set-car! (cdr env) val)
    (cont val))
   ;; otherwise keep looking in rest of environment
   (else (env-define-helper var val (cdr (cdr env)) cont fail))))



(define (env-set! var val env cont)
  (env-set-helper var val env cont (lambda (ignore)
				     (error "set! no binding for variable " var val env cont))))

(define (env-set-helper var val env cont fail)
  (cond
   ;; symbol not found -- fail
   ((null? env) (fail #t))
   ;; if symbol is in first  -- and succeed
   ((eq? var (car env))
    (set-car! (cdr env) val)
    (cont val))
   ;; otherwise keep looking in rest environment
   (else (env-set-helper var val (cdr (cdr env)) cont fail))))



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

;; dont use scheme display , we want our own pretty-printer 
(define (eval-display exp env cont)
  (let ((thing-part (car (cdr exp))))
    (base-eval thing-part env
	       (lambda (v1)
		 (pprint v1)
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

;; --------------------- add --------------------------------
(define eval-add-helper
  (lambda (xs sum cont)
    (cond
     ((null? xs) (cont sum))
     ((not (number? (car xs)))
      (debugger "+ not a number"))
     (else (eval-add-helper (cdr xs) (+ sum (car xs)) cont)))))

(define eval-add
  (lambda (operands env cont)
    (eval-add-helper operands 0 cont)))



;; (define eval-add-helper
;;   (lambda (operands env xs sum cont)
;;     (cond
;;      ((null? xs) (cont sum))
;;      ((not (number? (car xs)))
;;       (debugger "not a number" operands env cont))
;;      (else (eval-add-helper operands env (cdr xs) (+ sum (car xs)) cont)))))

;; (define eval-add-helper
;;   (lambda (xs sum)
;;     (cond
;;      ((null? xs) sum)
;;      (else (eval-add-helper (cdr xs) (+ sum (car xs)))))))

;; (define eval-add
;;   (lambda eargs    
;;     ;; (newline)
;;     ;; (display "+ EARGS : ")
;;     ;; (display eargs)
;;     ;; (newline)
;;     ;; (display "+ EARGS.LENGTH : ")    
;;     ;; (display (length eargs))
;;     ;; (newline)    
;;     (eval-add-helper eargs 0)))




;; (define (eval-add exp env cont)
;;   (cond
;;    ;; (+)  = 0
;;    ((null? (cdr exp)) (cont 0))
;;    ;; (+ x) = x 
;;    ((null? (cdr (cdr exp)))
;;     (base-eval (car (cdr exp))
;; 	       env
;; 	       cont))
;;    ((null? (cdr (cdr (cdr exp))))
;;     (base-eval (car (cdr exp))
;; 	       env
;; 	       (lambda (v1)
;; 		 (base-eval (car (cdr (cdr exp)))
;; 			    env
;; 			    (lambda (v2)
;; 			      (cont (+ v1 v2)))))))
;;    (else 
;;    ;; (+ x y)
;;    ;; (+ x y z)
;;    ;; (+ x y z a)
;;     ;; (+ x y z a b)
;;     (let ((expansion (eval-add-helper (cdr exp))))
;;       ;;(format #t "[+ expander] : ~a ~%" expansion)
;;       (base-eval expansion
;; 		 env
;; 		 cont)))))

;; (define (eval-add-helper vals)
;;   (cond
;;    ((null? vals) '())
;;    ((null? (cdr vals)) vals)
;;    ((null? (cdr (cdr vals))) (cons '+ vals))   
;;    (else (list '+
;; 	       (car vals)
;; 	       (eval-add-helper (cdr vals))))))

;; ---------------------------------------------------
(define eval-sub-helper
  (lambda (xs sum cont)
    (cond
     ((null? xs) (cont sum))
     ((not (number? (car xs)))
      (debugger "- not a number"))
     (else (eval-sub-helper (cdr xs) (- sum (car xs)) cont)))))

(define eval-sub
  (lambda (operands env cont)
    (cond
     ((null? operands) (cont 0))
     ((null? (cdr operands)) (cont (- 0 (car operands))))
     (else (eval-sub-helper (cdr operands) (car operands) cont)))))


;; --------------------- mul --------------------------------
(define eval-mul-helper
  (lambda (xs prod cont)
    (cond
     ((null? xs) (cont prod))
     ((not (number? (car xs)))
      (debugger "* not a number"))
     (else (eval-mul-helper (cdr xs) (* prod (car xs)) cont)))))

(define eval-mul
  (lambda (operands env cont)
    (cond
     ((null? operands) (cont 1))
     (else (eval-mul-helper operands 1 cont)))))




;; (define (eval-mul exp env cont)
;;   (cond
;;    ;; (*)  = 1
;;    ((null? (cdr exp)) (cont 1))
;;    ;; (* x) = x 
;;    ((null? (cdr (cdr exp)))
;;     (base-eval (car (cdr exp))
;; 	       env
;; 	       cont))
;;    ((null? (cdr (cdr (cdr exp))))
;;     (base-eval (car (cdr exp))
;; 	       env
;; 	       (lambda (v1)
;; 		 (base-eval (car (cdr (cdr exp)))
;; 			    env
;; 			    (lambda (v2)
;; 			      (cont (* v1 v2)))))))
;;    (else 
;;     ;; (* x y)
;;     ;; (* x y z)
;;     ;; (* x y z a)
;;     ;; (* x y z a b)
;;     (let ((expansion (eval-mul-helper (cdr exp))))
;;       ;;(format #t "[* expander] : ~a ~%" expansion)
;;       (base-eval expansion
;; 		 env
;; 		 cont)))))

;; (define (eval-mul-helper vals)
;;   (cond
;;    ((null? vals) '())
;;    ((null? (cdr vals)) vals)
;;    ((null? (cdr (cdr vals))) (cons '* vals))   
;;    (else (list '*
;; 	       (car vals)
;; 	       (eval-mul-helper (cdr vals))))))



;;---------------------------------------------------------
(define eval-div-helper
  (lambda (xs prod cont)
    (cond
     ((null? xs) (cont prod))
     ((not (number? (car xs)))
      (debugger "/ not a number"))
     (else (eval-div-helper (cdr xs) (/ prod (car xs)) cont)))))

(define eval-div
  (lambda (operands env cont)
    (cond
     ((null? operands) (debugger "/ division requires a number"))
     (else (eval-div-helper (cdr operands) (car operands) cont)))))






;; ;; (/)
;; ;; (/ X)
;; ;; (/ X Y)
;; ;; (/ X Y ...) multiple args ...
;; (define (eval-div exp env cont)
;;   (let ((a-part (car (cdr exp)))
;; 	(b-part (car (cdr (cdr exp)))))
;;     (base-eval a-part env
;; 	       (lambda (v1)
		 
;; 		 (base-eval b-part env
;; 			    (lambda (v2)
;; 			      (cont (/ v1 v2))))))))




;; (eq?)
;; (eq? X)
;; (eq? X Y)
;; (eq? X Y ...) .... multiple arguments ??
(define (eval-eq? exp env cont)
  (let ((a-part (car (cdr exp)))
	(b-part (car (cdr (cdr exp)))))
    (base-eval a-part env
	       (lambda (v1)
		 (base-eval b-part env
			    (lambda (v2)
			      (cont (eq? v1 v2))))))))



;;(eqv?)
;;(eqv? X)
;;(eqv? X Y)
;;(eqv? X Y Z) ... multiple
(define (eval-eqv? exp env cont)
  (let ((a-part (car (cdr exp)))
	(b-part (car (cdr (cdr exp)))))
    (base-eval a-part env
	       (lambda (v1)
		 (base-eval b-part env
			    (lambda (v2)
			      (cont (eqv? v1 v2))))))))




;;; lambda with symbol as params
;;; (lambda args ...)

;;; lambda with list params
;;; (lambda (x y z) ...)

;;; dotted lambda 
;;; (lambda (x y . z) ...) 


;;; (quote) .... malformed
;;; (quote X) .... ok
;;; (quote X ...) .... malformed - too many args
(define (eval-quote exp env cont)
   (cont (car (cdr exp))))


;;; (if) ..... malformed
;;; (if X) .... malformed
;;; (if X Y) .... ok
;;; (if X Y Z) ... ok
;;; (if X Y Z ...) ... malformed
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



;; (cons X Y) 
(define (eval-cons exp env cont)
  (let ((car-part (car (cdr exp)))
	(cdr-part (car (cdr (cdr exp)))))
    (base-eval car-part env
	       (lambda (v1)
		 (base-eval cdr-part env
			    (lambda (v2)
			      (cont (cons v1 v2))))))))



;; (car X) , expect X is a pair
(define (eval-car exp env cont)
  (let ((cons-part (car (cdr exp))))
    (base-eval cons-part env
	       (lambda (v1)
		 (if (pair? v1)
		     (begin
		       (cont (car v1)))
		     (begin
		       (debugger "CAR expected a pair" exp env cont)))))))


;; (cdr X) , expect X is a pair
(define (eval-cdr exp env cont)
  (let ((cons-part (car (cdr exp))))
    (base-eval cons-part env
	       (lambda (v1)
		 (if (pair? v1)
		     (begin
		       (cont (cdr v1)))
		     (begin
		       (debugger "CDR expects a pair" exp env cont)))))))







;; expose current environment to interpreter
;; pass underlying system objects up to interpreter
;; need to package these objects appropriately.
(define (eval-current-environment exp env cont)
  (cont (make-environment env)))




(define (eval-environment? exp env cont)
  (let ((env-part (car (cdr exp))))
    (base-eval env-part env
	       (lambda (v1)
		 (if (environment-tagged? v1)
		     (cont #t)
		     (cont #f))))))


;; evaluate 1st argument
;; evaluate expression once like everything
;; then we evaluate it again , thats the EVAL part
(define (eval-eval exp env cont)
  (let ((exp-part (car (cdr exp))))
    (base-eval exp-part env
	       (lambda (v1)
		 (base-eval v1 env
			    (lambda (v2)
			      (cont v2)))))))


;; multi-argument version is 
;; apply f x => (f x)
(define (eval-primitive-apply exp env cont)
  (let ((fun-part (car (cdr exp)))
	(arg-part (car (cdr (cdr exp)))))    
    (base-eval fun-part
    	       env
    	       (lambda (v1)
    		 (base-eval arg-part
    			    env
    			    (lambda (v2)
    			      (base-eval (cons v1 v2)
    					 env
    					 cont)))))))



;; evaluates a sequence of expressions
;; consing into last LIST expression , otherwise get improper list e.g (cons 'a 'b)
(define (eval-apply-sequence exp env cont)
  (cond
   ((null? exp) (cont '()))
   ((null? (cdr exp)) 
      (base-eval (car exp)
		 env
		 cont))
   (else
    (base-eval (car exp)
	       env
	       (lambda (v1)
		 (eval-apply-sequence (cdr exp)
				      env
				      (lambda (v2)
					(cont (cons v1 v2)))))))))


(define (eval-apply exp env cont)
  (display "EVAL_APPLY : ")
  ;;(display exp)
  (newline)  
  (let ((fun-part (car (cdr exp)))
	(args-part (cdr (cdr exp))))
    (eval-apply-sequence args-part
			 env
			 (lambda (operands)
			   (display "EVAL_APPLY : operands = ")
			   ;;(display operands)
			   ;;(newline)  			   
			   (base-eval fun-part
				      env
				      (lambda (operator)
					(display "EVAL_APPLY : operator = ")
					;;(display operator)
					(newline)
					(base-apply operator operands env cont)))))))






    ;; (let ((apply-exp (cons fun-part args-part)))
    ;;   (display "EVAL_APPLY : APPLY_EXP = ")
    ;;   (display apply-exp)
    ;;   (newline)
    ;;   (cont apply-exp))))
;;      (base-eval apply-exp
;;		 env
;;		 cont))))







		 ;; (eval-apply-helper args-part
		 ;; 		    env
		 ;; 		    (lambda (v2)
				      
		 ;; 		      (newline)
		 ;; 		      (display "APPLY : v1 = ")
		 ;; 		      ;;(display v1)
		 ;; 		      (newline)
		 ;; 		      (display "APPLY : v2 = ")
		 ;; 		      ;;(display v2)
		 ;; 		      (newline)
		 ;; 		      ;;(display "APPLY-EXP : v1 . v2 = ")
		 ;; 		      ;;(display (cons v1 v2))
		 ;; 		      (newline)
		 ;; 		      (base-eval (cons v1 v2)
		 ;; 				 env
		 ;; 				 cont)))))))




;; dont know about defmacro

;; ;; (defmacro (name x y . z) .... )
;; (define (eval-defmacro exp env cont)
;;   (let ((name (car (cdr exp)))
;; 	(body (cdr (cdr exp))))
;;     (base-eval macro-part env
;; 	       (lambda (v1)
;; 		 (cont (macro-expand v1))))))



;; (macro-expand x)
(define (eval-macro-expand exp env cont)
  (let ((macro-part (car (cdr exp))))
    (base-eval macro-part env
	       (lambda (v1)
		 (cont (macro-expand v1))))))




;; helper so multiple bindings to the same thing
;;(define callcc-singleton (cps-primitive eval-callcc))


(define cps-function-obj
  (lambda (x)
    (cdr x)))

(define cps-function
  (lambda (obj)
    (cons (cons 'function 'tag) obj)))

(define cps-function?
  (lambda (x)
    (and (pair? x)
	 (equal? (car x) (cons 'function 'tag)))))

     



;; some primitives
(define environment '())

(define install-procedure
  (lambda (name obj)
    (set! environment (cons name (cons obj environment)))))





;; ;; repl is just a loop , that is all it does -- for now.
;; (define (repl)
;;   (begin    
;;     (newline)    
;;     (display "::> ")
;;     (read-k (lambda (exp)
;; 	      ;; macro-expand the expression
;; 	      (let ((expanded-exp (macro-expand exp)))
;; 		(display "ME> ")
;; 		(pprint expanded-exp)
;; 		(newline)
;; 		;; evaluate the expanded macro free expression
;; 		(base-eval expanded-exp
;; 			   environment
;; 			   (lambda (val)
;; 			     (newline)
;; 			     (display "==> ")
;; 			     (pprint val)
;; 			     (newline)
;; 			     (repl))))))))



;;------------------------------------------


;;   'cps-primitive  cps-primitive
;;  'defmacro  (cps-primitive eval-defmacro)

;; (install-procedure 'eof-object? eof-object?)
;; (install-procedure 'primitive-procedure? procedure?)

;; (install-procedure 'macro-expand  (cps-primitive eval-macro-expand))

;; (install-procedure 'eval   (cps-primitive eval-eval))


;; ;;(install-procedure 'apply  (cps-primitive eval-apply))
;; ;;(install-procedure 'begin  (cps-primitive eval-begin))

;; (let ((cc (cps-primitive eval-callcc)))
;;   (install-procedure 'call/cc  cc)
;;   (install-procedure 'call-with-current-continuation  cc))

;; ;;(install-procedure 'quote  (cps-primitive eval-quote))
;; ;;(install-procedure 'if (cps-primitive eval-if))
;; ;;(install-procedure 'lambda (cps-primitive eval-lambda))

;; ;;(install-procedure 'let (cps-primitive eval-let))
;; ;;(install-procedure 'let*  (cps-primitive eval-let-star))
;; ;;(install-procedure 'letrec (cps-primitive eval-letrec))

;; ;;(install-procedure  'assoc (cps-primitive eval-assoc))
;; ;;(install-procedure  'set! (cps-primitive eval-set!))  

;; (install-procedure  'newline (cps-primitive eval-newline))
;; (install-procedure  'define (cps-primitive eval-define))

;; (install-procedure  'cons (cps-primitive eval-cons))
;; (install-procedure  'car (cps-primitive eval-car))
;; (install-procedure  'cdr (cps-primitive eval-cdr))

;; (install-procedure  '+ (cps-function eval-add))
;; (install-procedure  '- (cps-function eval-sub))	    
;; (install-procedure  '* (cps-function eval-mul))
;; (install-procedure  '/ (cps-function eval-div))

;; ;; NOTICE - BIG change - CPS-FUNCTION instead of CPS-PRIMITIVE !!

;; (install-procedure  'eq? (cps-primitive eval-eq?))
;; (install-procedure  'eqv? (cps-primitive eval-eqv?))
;; (install-procedure  '> (cps-primitive eval-greater-than))
;; (install-procedure  '< (cps-primitive eval-less-than))
;; (install-procedure  '= (cps-primitive eval-num-eq))
;; (install-procedure  'read (cps-primitive eval-read))
;; (install-procedure  'display (cps-primitive eval-display))
;; (install-procedure  'set-car! (cps-primitive eval-set-car!))
;; (install-procedure  'set-cdr! (cps-primitive eval-set-cdr!))
;; (install-procedure  'gensym (cps-primitive eval-gensym)) 
;; (install-procedure  'load (cps-primitive eval-load))
;; (install-procedure  'vector? (cps-primitive eval-vector?))
;; (install-procedure  'boolean? (cps-primitive eval-boolean?))
;; (install-procedure  'string? (cps-primitive eval-string?))
;; (install-procedure  'symbol? (cps-primitive eval-symbol?))
;; (install-procedure  'number? (cps-primitive eval-number?))
;; (install-procedure  'null? (cps-primitive eval-null?))
;; (install-procedure  'pair? (cps-primitive eval-pair?))



   ;; fexprs -- like runtime macros -- call by text
   ;; 
;;   'fexpr (cps-primitive eval-fexpr)
   
   ;; the very same environment
;;   'current-environment (cps-primitive eval-current-environment)
;;   'environment? (cps-primitive eval-environment?)    
   
   ;; repl continuation 
;;   'repl-cont   #t

   ;; map is no longer a primitive
   ;; 'map  (cps-primitive eval-map)
   
   ;;'and  (cps-primitive eval-and)  
   ;;'or  (cps-primitive eval-or)

  ;; cond is now a macro !!
  ;;'cond  (cps-primitive eval-cond)

  ;; quasiquote and macro expander  
  ;; quote is a useful primitive to have



;; ----------- SECTION 1 -----------------
;; section 1 should be



(define library
  (lambda (path)
    (string-append *installation-directory* path)))


;;quasiquotation
(load  	  "/home/terry/lisp/cps-interpreter/core/quasiquote.scm")

;; non hygienic macro expander
(load 	  "/home/terry/lisp/cps-interpreter/core/macro-expander.scm")

;; quasiquote expander
(load 	  "/home/terry/lisp/cps-interpreter/macros/quasiquote.scm")

;; cond is nested ifs
(load 	  "/home/terry/lisp/cps-interpreter/macros/cond.scm")

;; let is just applied lambda, but visual appearances easier keep as let
;; 
(load 	  "/home/terry/lisp/cps-interpreter/macros/let.scm")

;; letrec requires lets and sets
(load 	  "/home/terry/lisp/cps-interpreter/macros/letrec.scm")

;; let* are sequential lets
(load 	  "/home/terry/lisp/cps-interpreter/macros/let-star.scm")

;; when is just conditional with sequencing
(load 	  "/home/terry/lisp/cps-interpreter/macros/when.scm")

;; dont know if we need lambda expander , 
(load 	  "/home/terry/lisp/cps-interpreter/macros/lambda.scm")

;; conjunction
(load 	  "/home/terry/lisp/cps-interpreter/macros/and.scm")

;; disjunction
(load 	  "/home/terry/lisp/cps-interpreter/macros/or.scm")

;; define
(load 	  "/home/terry/lisp/cps-interpreter/macros/define.scm")


;; **** ------ SICP register machine ------- ****
;; **** this redefines REPL 
(load 	  "/home/terry/lisp/cps-interpreter/sicp.scm")

;; start the system
(repl)



;; ;; ----------- SECTION 2 -----------------
;; ;; section 2 should be loaded in the repl , so fib fac are defined
;; ;; some useful routines
;; (base-eval '(begin
;; 	      (load "/home/terry/lisp/cps-interpreter/util/not.scm")
;; 	      (load "/home/terry/lisp/cps-interpreter/util/list.scm")
;; 	      (load "/home/terry/lisp/cps-interpreter/util/append.scm")
;; 	      (load "/home/terry/lisp/cps-interpreter/util/length.scm")
;; 	      (load "/home/terry/lisp/cps-interpreter/util/reverse.scm")      
;; 	      (load "/home/terry/lisp/cps-interpreter/util/apply.scm")      
;; 	      (load "/home/terry/lisp/cps-interpreter/util/map.scm")
;; 	      (load "/home/terry/lisp/cps-interpreter/util/fac.scm")
;; 	      (load "/home/terry/lisp/cps-interpreter/util/fib.scm")
;; 	      (load "/home/terry/lisp/cps-interpreter/util/assoc.scm")
;; 	      ;; -- run the tests --
;; 	      (load "/home/terry/lisp/cps-interpreter/tests/apply.scm")	      
;; 	      (load "/home/terry/lisp/cps-interpreter/tests/map.scm")	      
;; 	      ;;(load "/home/terry/lisp/cps-interpreter/tests/fib.scm")
;; 	      (display "!! ALL LIBRARIES LOADED OKAY !!")
;; 	      (newline)
;; 	      )
;; 	   environment
;; 	   (lambda (k)
;; 	     ;; run the repl if everything went ok
;; 	     ;; ------ start interactive repl -----
;; 	     (repl)))

;; ------ run the tests ----------























