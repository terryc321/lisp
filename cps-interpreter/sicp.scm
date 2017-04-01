


;; sicp evaluator

;; 1 expression register
(define exp '())
;; 2 environment register
(define env '())
;; 3 argument list register
(define argl '())
(define argl-tail #f)
;; 4 unevaluated args register
(define unev '())
;; 5 procedure register
(define proc '())
;; 6 continue register
(define cont '())
;; 7 result register
(define val '())
;; the machine stack
(define stack '())

;; bug register - records what went wrong
(define bug '())


;; give TOPLEVEL some definitions
;; also required SET-CAR! , SET-CDR! because they dont work on empty list
;; see ev-definition-2 
(define *toplevel* '(#t #t #f #f))



(define *initial-input-port* #f)
(define *initial-output-port* #f)

;; a unqiue tag for closures
;;(define closure-tag (generate-uninterned-symbol "closure"))

(define *stack-max-height* 0)
(define *stack-min-height* 0)
(define *stack-pushes* 0)
(define *stack-pops* 0)
(define *stack-height* 0)


(define (stats-reset)
  (set! *stack-max-height* 0)
  (set! *stack-min-height* 0)
  (set! *stack-pushes* 0)
  (set! *stack-pops* 0)
  (set! *stack-height* 0))
  

(define (stats)
  (newline) (display "*------- stack statistics ---------*") (newline)
  (display "stack max height : ") (display *stack-max-height* ) (newline)
  (display "stack min height : ") (display *stack-min-height* ) (newline)
  (display "stack pushes : ") (display *stack-pushes* ) (newline)
  (display "stack pops   : ") (display *stack-pops* ) (newline)
  (display "stack height    : ") (display *stack-height* ) (newline)
  (display "stack height[2] : ") (display (length stack)) (newline))


(define (read-and-macro-expand)
  (macro-expand (read)))


  

(define (save reg)
  (set! *stack-height* (+ 1 *stack-height*))
  ;; record maximum stack height
  (if (> *stack-height*
	 *stack-max-height*)
      (set! *stack-max-height* *stack-height*)
      #f)
  (set! *stack-pushes* (+ 1 *stack-pushes*))
  (cond
   ((eq? reg 'exp) (set! stack (cons exp stack)))
   ((eq? reg 'env) (set! stack (cons env stack)))
   ((eq? reg 'argl) (set! stack (cons argl stack)))
   ((eq? reg 'argl-tail) (set! stack (cons argl-tail stack)))   
   ((eq? reg 'unev) (set! stack (cons unev stack)))
   ((eq? reg 'proc) (set! stack (cons proc stack)))
   ((eq? reg 'cont) (set! stack (cons cont stack)))
   ((eq? reg 'val) (set! stack (cons val stack)))
   (else (error "SAVE : register not recognised" reg))))


(define (restore reg)
  (begin
    (set! *stack-height* (- *stack-height* 1))
    (set! *stack-pops* (+ 1 *stack-pops*))
    ;; record minimum stack height
    (if (< *stack-height*
	   *stack-min-height*)
	(set! *stack-min-height* *stack-height*)
      #f)
    (cond
     ((eq? reg 'exp)  (set! exp (car stack)))
     ((eq? reg 'env)  (set! env (car stack)))
     ((eq? reg 'argl) (set! argl (car stack)))
     ((eq? reg 'argl-tail) (set! argl-tail (car stack)))     
     ((eq? reg 'unev) (set! unev (car stack)))
     ((eq? reg 'proc) (set! proc (car stack)))
     ((eq? reg 'cont) (set! cont (car stack)))
     ((eq? reg 'val)  (set! val (car stack)))
     (else (error "RESTORE : register not recognised" reg)))
    (set! stack (cdr stack))))




;; every routine is just a label

;; ;; reverse x 
;; ((and (pair? exp)
;;       (eq? (car exp) 'reverse)
;;       (not (null? (cdr exp))))
;;  (set! exp (cdr exp))    
;;  (eval-reverse))

;; ;; pair? x 
;; ((and (pair? exp)
;;       (eq? (car exp) 'pair?)
;;       (not (null? (cdr exp))))
;;  (set! exp (cdr exp))    
;;  (eval-pair-p))

;; ;; list ....
;; ((and (pair? exp)
;;       (eq? (car exp) 'list))
;;  (set! exp (cdr exp))    
;;  (eval-list))   

;; ;; cons x y
;; ((and (pair? exp)
;;       (eq? (car exp) 'cons)
;;       (not (null? (cdr (cdr exp)))))
;;  (set! exp (cdr exp))
;;  (eval-cons))   

;; ;; car x
;; ((and (pair? exp)  (eq? (car exp) 'car))
;;  (set! exp (cdr exp))   
;;  (eval-car))

;; ;; cdr x
;; ((and (pair? exp)  (eq? (car exp) 'cdr))
;;  (set! exp (cdr exp))    
;;  (eval-cdr))

;; ;; (procedure? p)
;; ((and (pair? exp)
;;       (eq? (car exp) 'procedure?)
;;       (not (null? (cdr exp))))
;;  (set! exp (cdr exp))    
;;  (eval-procedure-p))


;; load up expression register
;; load up environment register
;; load up cont register
;; call sicp
;; evaluation continues until completed , or error
(define (base-eval)
  ;; (display "[sicp]BASE-EVAL:")
  ;; (pprint exp)
  ;; (newline)

  (cond

   ;; character = itself
   ((char? exp)
    (set! val exp)
    (cont))

   ;; string = itself
   ((string? exp)
    (set! val exp)
    (cont))

   
   ;; number = number
   ((number? exp)
    (set! val exp)
    (cont))

   ;; boolean = boolean
   ((boolean? exp)
    (set! val exp)
    (cont))

   ;; ;; microcode routines 
   ;; ((microcode-procedure? exp)
   ;;  (set! val exp)
   ;;  (cont))
   
   ;; machine routines are just code
   ((primitive-procedure? exp)
    (set! val exp)
    (cont))
   
   ;; closure = closure
   ((user-procedure? exp)
    (set! val exp)    
    (cont))
   
   ;; bye bye --- escape hatch 
   ((or (eq? exp 'bye)
	(and (pair? exp)
	     (eq? (car exp) 'bye)))
    (display "bye bye")
    #f)   
   
   ;; lookup
   ((symbol? exp)
    (set! val (lookup-symbol exp env))
    (cont))

   ;; if
   ((and (pair? exp)
         (eq? (car exp) 'if))    
    (ev-if))
   
   ;; quote
   ((and (pair? exp)
         (eq? (car exp) 'quote)
         (not (null? (cdr exp))))
    (set! val (car (cdr exp)))
    (cont))

   ;; begin expressions
   ((and (pair? exp)
         (eq? (car exp) 'begin))         
    (ev-begin))
   
   
   ;; (lambda args body)
   ((and (pair? exp)
         (eq? (car exp) 'lambda))
    (ev-lambda))
   
   ;; (define f x)
   ((and (pair? exp)
         (eq? (car exp) 'define)
         (not (null? (cdr (cdr exp))))
         (symbol? (car (cdr exp))))
    (ev-definition))

   ;; (set! x y)
   ((and (pair? exp)
         (eq? (car exp) 'set!)
         (not (null? (cdr (cdr exp))))
         (symbol? (car (cdr exp))))
    (ev-assignment)) 

   ;; try application
   ((pair? exp)
    (ev-application))
   
   (else
    (eval-error "BASE-EVAL : do not understand " exp)
    )))



;;eval-application ... evaluate all arguments ... then the procedure ...
;; if the procedure is compound - then just call it with argl
;; otherwise its a user defined function in language and pass parameters as required and
;; evaluate body of lambda in environment of the lambda yada yada yada...

;; eval-dispatch is just base-eval
(define (eval-dispatch)
  (base-eval))



;; (define f x)
(define (ev-definition)
  (set! unev (car (cdr exp))) ;; variable symbol 
  (save 'unev)   
  (set! exp (car (cdr (cdr exp)))) ;; arg x to eval
  (save 'env)
  (save 'cont)
  (set! cont ev-definition-2)
  (eval-dispatch))

(define (ev-definition-2)
  (restore 'cont)
  (restore 'env)
  (restore 'unev)
  ;; define the variable in environment
  ;; key = unev
  ;; value = val
  ;;   rest environment is env
  ;;(set! env (cons unev (cons val env)))

  (let ((old-car (car env))
	(old-cdr (cdr env)))
    
    ;; Lets ASSUME - no such definition in the environment 
    (set-car! env unev)
    (set-cdr! env (cons val (cons old-car old-cdr))))
  
  (set! val 'ok)
  (cont))

  

;; *****************************************

;; (set! a y)
(define (ev-assignment)
  (set! unev (car (cdr exp))) ;; variable symbol 
  (save 'unev)   
  (set! exp (car (cdr (cdr exp)))) ;; arg x to eval
  (save 'env)
  (save 'cont)
  (set! cont ev-assignment-2)
  (eval-dispatch))

(define (ev-assignment-2)
  (restore 'cont)
  (restore 'env)
  (restore 'unev)
  (set-variable-value unev val env)
  (set! val 'ok)
  (cont))




(define (set-variable-value k v xs)
  (cond
   ((null? xs) (eval-error "SET! : no binding for variable " k v))
   ((eq? k (car xs))
    (set-car! (cdr xs) v))
   (else (set-variable-value k v (cdr (cdr xs))))))






(define (ev-application)
  (set! unev (cdr exp))
  (save 'unev)
  (save 'env)
  (save 'cont)
  (set! exp (car exp))
  (set! cont ev-application-2)
  (eval-dispatch))

(define (ev-application-2)
  (restore 'cont)
  (restore 'env)
  (restore 'unev)
  (set! proc val)  
  (save 'proc)
  (save 'env)
  (save 'cont)
  (set! cont ev-application-3)
  (set! argl '())
  (ev-operands))

(define (ev-application-3)
  (restore 'cont)
  (restore 'env)
  (restore 'proc)  
  ;; (display "EV-APPLIcaTION-3 : PROC = ")
  ;; (display proc)
  ;; (newline)  
  ;; (display "EV-APPLIcaTION-3 : ARGL = ")
  ;; (display argl)
  ;; (newline)  
  ;;(cont)
  (apply-dispatch))


;; **** reverse *****

(define (ev-reverse)
  (set! unev (car (cdr exp)))
  (save 'cont)
  (set! cont ev-reverse-2)
  (ev-operands))

(define (ev-reverse-2)
  (restore 'cont)
  (set! val argl)
  (cont))

;; *******************

;; ev-operands requires a FRESH list as the arguments are SET-CAR'd 
(define (ev-operands)
  (set! argl (list 'dummy))
  (set! argl-tail argl)
  (ev-operands-2))


(define (ev-operands-2)
  (if (null? unev)
      (begin
	;; disgard dummy argl
	(set! argl (cdr argl))
	(cont))
      (begin
        (set! exp (car unev))
        (set! unev (cdr unev))
        (save 'env)
        (save 'unev)
        (save 'cont)
        (save 'argl)
	(save 'argl-tail)	
        (set! cont ev-operands-3)
        (eval-dispatch))))

(define (ev-operands-3)
  (restore 'argl-tail)  
  (restore 'argl)
  (restore 'cont)
  (restore 'unev)
  (restore 'env)
  (set-cdr! argl-tail (cons val '()))
  (set! argl-tail (cdr argl-tail))
  (ev-operands-2))










(define (apply-dispatch)
  ;;(bkpt 'apply-dispatch 'proc proc 'args argl)
  (cond
   ((primitive-procedure? proc)
    (primitive-apply))
   ((user-procedure? proc)
    (user-apply))
   ;; ((microcode-procedure? proc)
   ;;  (microcode-apply))
   (else
    (display "***INTERNAL ERROR **SICP APPLY-DISPATCH INTERNAL ERROR ")
    (newline)
    (display "PROC = ")
    (display proc)
    (newline)
    (eval-error "unknown procedure type"))))









(define primitive-procedure? compound-procedure?)

(define (primitive-apply)
  ;; apply primitive procedure with operator in PROC , operands in ARGL
  ;; put args in correct order visually
  
  ;;(set! argl (reverse argl))

  ;; (newline)
  ;; (display " * PRIMITIVE-APPLY * : ")
  ;; (display proc)
  ;; (display " : ARGS = ")
  ;; (display argl)
  ;; (newline)
  (save 'cont)
  (set! val (proc))
  (restore 'cont)
  (cont))


;; (define (microcode-apply)
;;   (newline)
;;   (display " * MICROCODE-APPLY * : ")
;;   (display proc)
;;   (display " : ARGS = ")
;;   (display argl)
;;   (newline)  
;;   (restore 'cont)
;;   (cont))


(define (user-apply)
  ;; procedure in PROC register

  
  ;; put arguments in correct order
  ;;(set! argl (reverse argl))


  ;;(bkpt 'user-apply 'proc proc)
  (set! unev (user-procedure-lambda-params proc))
  ;; call the lambda environment because its just a thunk with environment hidden inside .
  (set! env (user-procedure-lambda-env proc))
  ;; extend environment
  ;;(bkpt 'user-apply 'extending environment 'params 'unev unev 'vals 'argl argl)
  (extend-environment unev argl)
  ;; lambda body
  (set! unev (user-procedure-lambda-body proc))
  ;; implicit begin on body
  ;;(bkpt 'user-apply 'lambda-body= 'unev unev)
  (ev-sequence))




(define (ev-begin)
  (set! unev (cdr exp))  
  (ev-sequence))

(define (ev-sequence)
  (if (null? unev)
      (begin
	(set! val #f)	
	(cont))
      (begin
	(set! exp (car unev))  
	(if (null? (cdr unev))
	    (begin
	      (ev-sequence-3))
	    (begin
	      (save 'cont)
	      (save 'env)
	      (save 'unev)	      
	      (set! cont ev-sequence-2)
	      (eval-dispatch))))))

(define (ev-sequence-2)
  (restore 'unev)
  (restore 'env)
  (restore 'cont)
  (set! unev (cdr unev))
  (ev-sequence))

(define (ev-sequence-3)
  (eval-dispatch))


;; *****************************************

(define (ev-if)
  (set! unev (cdr (cdr exp)))
  (save 'unev)
  (save 'env)
  (save 'cont)
  (set! exp (car (cdr exp)))
  (set! cont ev-if-2)  
  (eval-dispatch))

(define (ev-if-2)
  (restore 'cont)
  (restore 'env)
  (restore 'unev)
  (if val
      (ev-if-3)
      (ev-if-4)))

(define (ev-if-3)
  (set! exp (car unev))
  (eval-dispatch))

;; (if X Y Z)
(define (ev-if-4)
  (set! unev (cdr unev))
  (if (null? unev)
      (begin
	;; no alternative clause
	(set! val #f)
	(cont))
      (begin
        (set! exp (car unev))
	(eval-dispatch))))







;; (define (eval-application-3)  
;;   (restore 'exp)
;;   (restore 'cont)
;;   ;; exp : operands 
;;   ;; val : operator
;;   (newline)
;;   (display " * EVAL-APPLICATION-3 * : ")
;;   (display val)
;;   (display " : ARGS ")
;;   (display exp)
;;   (newline)
;;   (cond
;;    ((compound-procedure? val)
;;     ;; disgard procedure from expression , leaving only arguments
;;     ;; call the primitive procedure
;;     (val))
;;    ((user-procedure? val)
;;     ;; pass parameters using only registers
;;     ;; assume simple (f x ) type stuff for now
;;     (newline)
;;     (display " * EVAL-APPLICATION-3 * USERs PROC : ")
;;     (display (user-procedure-lambda val))
;;     (newline)    
;;     (display " * EVAL-APPLICATION-3 * USERs PARAMS : ")
;;     (display (user-procedure-lambda-params val))
;;     (newline)    
;;     (display " * EVAL-APPLICATION-3 * USERs BODY : ")
;;     (display (user-procedure-lambda-body val))
;;     ;;
;;     (save 'env)
;;     (save 'cont)
;;     (set! cont eval-application-4)
   
;;     ;; re-install lambda environment
;;     ;; call the lambda environment because its just a thunk with environment hidden inside .
;;     (set! env ((user-procedure-lambda-env val)))
;;     ;; extend environment
;;     (extend-environment (user-procedure-lambda-params val) exp)
;;     ;; 
;;     (set! exp (user-procedure-lambda-body val))
;;     ;; do implicit begin on 
;;     (eval-begin))
;;    (else
;;     (eval-error "EVAL-APPLICATION-3 : does not understand" val exp))))


(define (extend-environment xs vals)
  (cond
   ((null? xs) 'done)
   ((symbol? xs)
    ;; (lambda xs ....)  xs gets all arguments
    ;; also handles dotted pair 
    (set! env (cons xs (cons vals env))))
   (else
    (begin
      ;; key = car xs
      ;; val = car vals
      ;;   env is rest of environment
      (set! env (cons (car xs) (cons (car vals) env)))
      (extend-environment (cdr xs) (cdr vals))))))





(define (eval-application-4)
  (restore 'cont)
  (restore 'env)
  (cont))
  




;; debugger
(define (eval-error msg . args)
  (newline)
  (display ";; ERROR : ")
  (display msg)
  (newline)
  (display ";; ERROR ARGS : ")
  (display args)
  (newline)  
  (set! bug (cons (list msg exp env cont) bug))
  (set! val (vector 'ERROR))
  (eval-debugger))


(define (eval-debugger)
  (display ";; DE-BUG [")
  (display (length bug))
  (display "] >")
  (newline)
  (save 'cont)
  
  (set! cont eval-debugger-print)
  (set! exp (read-and-macro-expand))
  ;; intercept expressions - in addition to base eval
  (cond
   ;; (continue x)
   ((and (pair? exp)
         (eq? (car exp) 'continue)
         (not (null? (cdr exp))))
    
    (set! cont eval-debugger-continue)
    (set! exp (car (cdr exp)))
    (base-eval))
   (else
    ;; usual eval
    (base-eval))))


(define (eval-debugger-print)
  (restore 'cont)
  (display ";; DE-BUG Value : ")
  (write val)
  (newline)
  (eval-debugger))


(define (eval-debugger-continue)
  (restore 'cont)
  ;; dropping one debugger frame then ...
  (set! bug (cdr bug))

  (display ";; DE-BUG stack : ")
  (display stack)
  (display ";; DE-BUG bug : ")
  (display bug)
  
  (display ";; DE-BUG Continue Value : ")
  (write val)
  (newline)
  (cont))


;; read - eval - print loop
;; if read fails 
(define (repl)
  (newline)
  (display ";; SICP >")

  (newline)
  (display ";;repl.stack >")
  (display stack)
  (newline)
  
  ;; save environment
  ;;(set! env *toplevel*)
  (save 'env)


 
  ;; (if current-input-port
  ;;     (begin #f)  ;; no action needed
  ;;     (begin (set! *initial-input-port*  current-input-port)))  
  
  ;;(stats-reset) ;; reset statistics
  
  (set! cont repl-print)
  (set! exp (read))
  (if (eof-object? exp)
      #t
      (begin
	(set! exp (macro-expand exp))
	(base-eval))))

(define (repl-print)
  (newline)
  (display ";; Value : ")
  (write val)
  (newline)

  ;;(stats) ;; show the stack statistics
  
  ;; restore environment
  ;;(set! *toplevel* env)
  (restore 'env)
  
  (repl))






;; ;; load-repl
;; (define (load-repl)
;;   (newline)
;;   (display ";; Load SICP >")
;;   (newline)
;;   (stats-reset) ;; reset statistics
;;   (set! cont load-repl-print)  
;;   (set! exp (read (current-input-port)))
;;   (if (eof-object? exp)
;;       (begin
;; 	(restore 'cont)
;; 	(goto 'cont))
;;       (begin
;; 	(set! exp (macro-expand exp))
;; 	(newline)
;; 	(display ";; Load EXP >")
;; 	(pprint exp)
;; 	(base-eval))))




;; (define (load-repl-print)
;;   (newline)
;;   (display ";; Load VAL : ")
;;   (write val)
;;   (load-repl))


;; (define (lookup-helper exp names values)
;;   (cond
;;    ((null? names)
;;     (eval-error "symbol not found" exp))
;;    ((eq? exp (car names))
;;     (car values))
;;    (else (lookup-helper exp (cdr names) (cdr values)))))


;; just a helper routine , not a continuation point
(define (lookup-symbol exp env)
  (cond
   ((null? env)
    (eval-error "symbol not found" exp))
   ((eq? exp (car env))
    (car (cdr env)))
   (else (lookup-symbol exp (cdr (cdr env))))))


;; ;; QUOTE : exp = (x)
;; (define (eval-quote)
;;   (set! val (car exp))
;;   (cont))


;; ;; DEFINE : exp = (f x)
;; (define (eval-define)
;;   (save 'cont)
;;   (save 'exp)
;;   (set! exp (car (cdr exp)))
;;   (set! cont eval-define-2)
;;   (base-eval))

;; (define (eval-define-2)  
;;   (restore 'exp)
;;   (restore 'cont)  
;;   (set! exp (car exp))
;;   ;; mutate environment with exp -> val
;;   ;; toplevel defines just extend environment
;;   (set! env (cons exp (cons val env)))
;;   (cont))


;; exp = (lambda ...)
;; wrap environment in a lambda so it is printable without spewing forth
(define (ev-lambda)
  ;; lambda parameters
  (set! unev (car (cdr exp)))
  ;; lambda body
  (set! exp  (cdr (cdr exp)))
  ;; make procedure
  ;;(bkpt 'making-procedure 'unev= unev 'exp= exp)
  (set! val (vector 'closure unev exp (lambda () env)))
  ;; continue
  (cont))

(define (user-procedure? x)
  (and (vector? x)
       (eq? 'closure (vector-ref x 0)))) 


(define (user-procedure-lambda-params x)
  (vector-ref x 1))

(define (user-procedure-lambda-body x)
  (vector-ref x 2))


(define (user-procedure-lambda-env x)
  (let ((e (vector-ref x 3)))
    (e)))


;; **************************************************
(define (microcode-procedure? x)
  (and (vector? x)
       (eq? 'microcode (vector-ref x 0)))) 

(define (microcode-routine x)
  (vector-ref x 1))

(define (make-microcode x)
  (vector 'microcode x))

;; **************************************************





;; ;; IF 
;; (define (eval-if)
;;   (save 'cont)
;;   (save 'exp)
;;   (set! exp (car exp))
;;   (set! cont eval-if-2)
;;   (base-eval))

;; (define (eval-if-2)
;;   (restore 'exp)
;;   (restore 'cont)
;;   (if val
;;       (begin
;; 	(set! exp (car (cdr exp)))
;; 	(base-eval))
;;       (begin
;; 	(if (null? (cdr (cdr exp)))
;; 	    (begin
;; 	      (set! val #f)
;; 	      (cont))
;; 	    (begin
;; 	      (set! exp (car (cdr (cdr exp))))
;; 	      (base-eval))))))

 


;; ;; BEGIN : exp = (....)
;; (define (eval-begin)
;;   (if (null? exp)
;;       ;; no value -- val undefined
;;       (begin
;; 	(set! val #f)
;; 	(cont))
;;       (begin
;; 	(if (null? (cdr exp))
;; 	    (begin
;; 	      (set! exp (car exp))
;; 	      (base-eval))
;; 	    (begin
;; 	      (save 'cont)
;; 	      (save 'exp)
;; 	      (set! exp (car exp))
;; 	      (set! cont eval-begin-2)
;; 	      (base-eval))))))

;; (define (eval-begin-2)
;;   (restore 'exp)
;;   (restore 'cont)
;;   (set! exp (cdr exp))
;;   (if (null? (cdr exp))
;;       (begin
;; 	;; last expression
;; 	(set! exp (car exp))
;; 	(base-eval))
;;       (begin
;; 	;; more expressions
;; 	(save 'cont)
;; 	(save 'exp)
;; 	(set! exp (car exp))
;; 	(set! cont eval-begin-2)
;; 	(base-eval))))


;; ;; exp = (...)
;; (define (eval-list-sequence)
;;   (if (null? exp)
;;       (begin
;;         (set! val '())
;;         (cont))
;;       (begin
;;         (save 'argl) ;; why??        
;;         (save 'cont)
;;         (save 'exp)
;;         ;; start new argument list
;;         (set! argl '())
;;         ;; 
;;         (set! exp (car exp))
;;         (set! cont eval-list-sequence-2)
;;         (base-eval))))
      
;; (define (eval-list-sequence-2)
;;   (set! argl (cons val argl))
;;   (restore 'exp)
;;   (set! exp (cdr exp))  
;;   (if (null? exp)
;;       (begin ;; no more to eval
;;         ;; arguments are actually in reverse
;;         (set! val (reverse argl))
;;         ;; 
;;         (restore 'cont)
;;         (restore 'argl)
;;         (cont))
;;       (begin ;; keep going
;;         (save 'exp)
;;         (set! exp (car exp))
;;         ;; assume cont is eval-list-sequence-2 again
;;         (base-eval))))




;; ************************************************************
;; primitives are normal functions that return a result
;; ************************************************************
;; ;; apply f arg1 arg2 arg3 .... arglist
;; ;;      (f arg1 arg2 arg3 .... ,@arglist )
(define (eval-apply-helper xs)
  (cond
   ((null? xs) xs)
   ((null? (cdr xs)) (car xs))
   (else (cons (car xs)
	       (eval-apply-helper (cdr xs))))))

(define (eval-apply)  
  (newline)
  (display "eval-apply: PROC=")
  (display proc)
  (newline)
  (display "eval-apply: ARGL=")
  (display argl)
  (newline)
  
  (set! proc (car argl))
  (set! argl (eval-apply-helper (cdr argl)))

  (newline)
  (display "eval-apply after: PROC=")
  (display proc)
  (newline)
  (display "eval-apply after: ARGL=")
  (display argl)
  (newline)
  
  (save 'cont)
  (set! cont eval-apply-2)
  (apply-dispatch))




(define (eval-apply-2)
  (restore 'cont)

  (newline)
  (display "eval-apply .after : STACK=")
  (display stack)
  (newline)
  (display "eval-apply .after : VAL=")
  (display val)
  (newline)
  val)










  







;; CONS : argl = (x y)
;; evaluate x , save val ... later restore as argl ... then do cons
(define (eval-cons)  
  (if (< (length argl) 2)
      (eval-error "CONS : insufficient args")
      #f)
  (if (> (length argl) 2)
      (eval-error "CONS : too many args")
      #f)  
  (set! val (cons (car argl) (car (cdr argl))))
  val)

;; PROCEDURE? : argl = (f)
(define (eval-procedure-p)
  (if (< (length argl) 1)
      (eval-error "procedure? : insufficient args")
      (or (user-procedure? (car argl))
          (primitive-procedure? (car argl)))))



;; BOOLEAN? : exp = (f)
(define (eval-boolean-p)
  (if (< (length argl) 1)
      (eval-error "boolean? : insufficient args")
      (boolean? (car argl))))


(define (eval-symbol-p)
  (if (< (length argl) 1)
      (eval-error "symbol? : insufficient args")
      (symbol? (car argl))))

(define (eval-number-p)
  (if (< (length argl) 1)
      (eval-error "number? : insufficient args")
      (number? (car argl))))


;; PAIR? : exp = (f)
(define (eval-pair-p)
  (if (< (length argl) 1)
      (eval-error "pair? : insufficient args")
      (pair? (car argl))))



;; CAR : exp = (x)
(define (eval-car)
  (cond
   ((< (length argl) 1)
      (eval-error "CAR : insufficient args"))
   ((> (length argl) 1)
    (eval-error "CAR : too many args"))
   ((not (pair? (car argl)))
    (eval-error "CAR : expected a pair"))
   (else (car (car argl)))))

;; CDR
(define (eval-cdr)
  (cond
   ((< (length argl) 1)
      (eval-error "CDR : insufficient args"))
   ((> (length argl) 1)
    (eval-error "CDR : too many args"))
   ((not (pair? (car argl)))
    (eval-error "CDR : expected a pair"))
   (else (cdr (car argl)))))

;; REVERSE : exp = (xs)
(define (eval-reverse)
  (if (and (pair? argl) (pair? (car argl)))
      (reverse (car argl))
      (begin ;; otherwise
        (eval-error "REVERSE : not a pair"))))


;; LIST : exp = (...)
(define (eval-list)  argl)

;; APPEND
(define (eval-append)
  (define (append-helper xs rs)
    (cond
     ((null? xs) rs)
     (else (append-helper (cdr xs) (append rs (car xs))))))
  (append-helper argl '()))


;; LENGTH : exp = (xs)
(define (eval-length)
  (if (and (pair? argl) (or (null? (car argl))
			   (pair? (car argl))))
      (length (car argl))
      (begin ;; otherwise
        (eval-error "LENGTH : not a pair or empty list"))))



;; EQ? : exp = (....)
(define (eval-eq-p)
  (if (and (pair? argl)
	   (pair? (cdr argl)))
      (eq? (car argl) (car (cdr argl)))
      (begin ;; otherwise
        (eval-error "EQ? : expected 2 arguments"))))

;; EQV? : exp = (....)
(define (eval-eqv-p)
  (if (and (pair? argl) (pair? (cdr argl)))
      (eqv? (car argl) (car (cdr argl)))
      (begin ;; otherwise
        (eval-error "EQV? : expected 2 arguments"))))


;; = : exp = (....)
(define (eval-num-eq)
  ;; (display "NUM = : ")
  ;; (display argl)
  ;; (newline)
  (if (and (pair? argl)
	   (pair? (cdr argl)))
      (begin
	(if (or (not (number? (car argl)))
		(not (number? (car (cdr argl)))))
	    (eval-error "NUM = : not a number")
            (if (= (car argl)
                   (car (cdr argl)))
                (begin
                  ;; (1 2 3)
                  ;; 
                  (if (null? (cdr (cdr argl)))
                      (begin
                        #t)
                      (begin
                        (set! argl (cdr argl))
                        (eval-num-eq))))
                (begin
                  #f))))
      (begin ;; otherwise
        (eval-error "NUM = : expected 2 arguments"))))


;; + : exp = (....)
(define (eval-num-add-helper xs)
  (cond
   ((null? xs) val)
   ((number? (car xs))
    (set! val (+ val (car xs)))
    (eval-num-add-helper (cdr xs)))
   (else
    (eval-error "NUM + : not a number"))))
   
(define (eval-num-add)
  (set! val 0)
  (eval-num-add-helper argl))



;; * : exp = (....)
(define (eval-num-mul-helper xs)
  (cond
   ((null? xs) val)
   ((number? (car xs))
    (set! val (* val (car xs)))
    (eval-num-mul-helper (cdr xs)))
   (else
    (eval-error "NUM * : not a number"))))
   
(define (eval-num-mul)
  (set! val 1)
  (eval-num-mul-helper argl))






;; - : exp = (....)
(define (eval-num-sub-helper xs)
  (cond
   ((null? xs) val)
   ((number? (car xs))
    (set! val (- val (car xs)))
    (eval-num-sub-helper (cdr xs)))
   (else
    (eval-error "NUM - : not a number"))))
   
(define (eval-num-sub)
  (set! val 0)
  (cond
   ((null? argl) (eval-error "NUM - : expected 1 argument"))
   ((and (null? (cdr argl))
	 (number? (car argl)))
    (set! val (- 0 (car argl)))
    val)
   ((null? (cdr argl))
    (eval-error "NUM - : expected a number"))    
   (else
    (set! val (car argl))
    (eval-num-sub-helper (cdr argl)))))




;; / : exp = (....)
(define (eval-num-div-helper xs)
  (cond
   ((null? xs) val)
   ((number? (car xs))
    (set! val (/ val (car xs)))
    (eval-num-div-helper (cdr xs)))
   (else
    (eval-error "NUM / : not a number"))))
   
(define (eval-num-div)
  (set! val 0)
  (cond
   ((null? argl) (eval-error "NUM / : expected 1 argument"))   
   ((and (null? (cdr argl))
	 (number? (car argl)))
    (set! val (/ 1 (car argl)))
    val)
   ((null? (cdr argl))
    (eval-error "NUM / : expected a number"))
   (else
    (set! val (car argl))
    (eval-num-div-helper (cdr argl)))))


;; ;; open file , keep reading in stuff , close file ,
;; ;; return last result to val register
;; (define (eval-load)  
;;   (set! val #f)
;;   (let ((filename (car argl)))
;;     (let ((port (open-input-file filename)))
;;       (set! *initial-input-port*  current-input-port)
;;       (set-current-input-port! port))))


;; filename in argl 
(define (eval-load)
  (set! val #f)
  (let ((filename (car argl)))
    (with-input-from-file filename
      (lambda ()	
	(load-repl)))))

	
	




;; read - eval - print loop
;; if read fails 
(define (load-repl)
  ;; (newline)
  ;; (display ";; LOAD-SICP >")
  ;; (newline)
  
  ;; save environment
  ;;(set! env *toplevel*)
  (save 'cont)
  (save 'env)
  
  ;; (if current-input-port
  ;;     (begin #f)  ;; no action needed
  ;;     (begin (set! *initial-input-port*  current-input-port)))  
  
  ;;(stats-reset) ;; reset statistics
  
  (set! cont load-repl-print)
  (set! exp (read))
  (if (eof-object? exp)
      (begin
	(restore 'env)
	(restore 'cont)
	(cont))
      (begin
	;; (newline)
	;; (display ";; LOAD-SICP >")
	;; (display exp)
	;; (newline)
	(set! exp (macro-expand exp))

	;; (newline)
	;; (display ";; LOAD-SICP : ME >")
	;; (newline)
	;; (display exp)
	
	(base-eval))))


(define (load-repl-print)
  ;; (newline)
  ;; (display ";; LOAD-Value : ")
  ;; (write val)
  ;; (newline)

  ;; (stats) ;; show the stack statistics
  
  ;; restore environment
  ;;(set! *toplevel* env)
  (restore 'env)
  (restore 'cont)
  (load-repl))












  





;;*****************************************************************
;; primitives should really just return SOME VALUE , which is eventally assigned to VAL register
;; or little val register to be more precise.
(define (eval-read-line) (read-line))
(define (eval-read-char) (read-char))
(define (eval-char-p) (apply char? argl))
(define (eval-char-eq) (apply char=? argl))

(define (eval-char-ci=?) (apply char-ci=? argl))
(define (eval-string->list) (apply string->list argl))

(define (eval-string-p) (apply string? argl))

(define (eval-gensym) (apply generate-uninterned-symbol argl))
(define (eval-symbol->string) (apply symbol->string argl))
(define (eval-null-p) (apply null? argl))
(define (eval-memq) (apply memq argl))

(define (eval-display) (apply display argl))
(define (eval-newline) (apply newline argl))



;; (define (eval-cadr) (apply cadr argl))
;; (define (eval-cddr) (apply cddr argl))
;; (define (eval-caadr) (apply caadr argl))
;; (define (eval-cdadr) (apply cdadr argl))


;; ;; (apply f a b c '(d e f))
;; ;; last item should be a list
;; ;; APPLY : exp = (f a b c '(d e f))
;; (define (eval-apply)
;;   (save 'cont)
;;   (set! exp (car exp))
;;   (set! cont eval-reverse-2)
;;   (base-eval))

;; (define (eval-apply-2)
;;   (restore 'cont)
;;   (if (pair? val)
;;       (set! val (reverse val))
;;       (begin ;; otherwise
;;         (eval-error "REVERSE : not a pair")))
;;   (cont))


;; have some initial environment ...
;;*****************************************************************




(define (install-toplevel name val)
  (set! *toplevel* (cons name (cons val *toplevel*))))



(install-toplevel 'reverse eval-reverse)
(install-toplevel 'list eval-list)
(install-toplevel 'append eval-append)
(install-toplevel 'car eval-car)
(install-toplevel 'cdr eval-cdr)
(install-toplevel 'cons eval-cons)
(install-toplevel 'procedure? eval-procedure-p)
(install-toplevel 'pair? eval-pair-p)
(install-toplevel 'boolean? eval-boolean-p)
(install-toplevel 'symbol? eval-symbol-p)
(install-toplevel 'number? eval-number-p)

(install-toplevel 'apply eval-apply)

(install-toplevel 'length eval-length)
(install-toplevel 'eq? eval-eq-p)
(install-toplevel 'eqv  eval-eqv-p)
(install-toplevel '=  eval-num-eq)
(install-toplevel '+  eval-num-add)
(install-toplevel '*  eval-num-mul)
(install-toplevel '-  eval-num-sub)
(install-toplevel '/  eval-num-div)
(install-toplevel 'read-line eval-read-line)
(install-toplevel 'read-char eval-read-char)
(install-toplevel 'char?  eval-char-p)
(install-toplevel 'char=?  eval-char-eq)
(install-toplevel 'char-ci=?  eval-char-ci=?)
(install-toplevel 'string->list  eval-string->list)
(install-toplevel 'load  eval-load)
(install-toplevel 'string?  eval-string-p)
(install-toplevel 'gensym  eval-gensym)
(install-toplevel 'symbol->string  eval-symbol->string)
(install-toplevel 'null?  eval-null-p)
(install-toplevel 'memq   eval-memq)
(install-toplevel 'display eval-display)
(install-toplevel 'newline eval-newline)

;; (newline)
;; (display "*toplevel* = ")
;; (display *toplevel*)
;; (newline)

;; an initial environment          
(set! env *toplevel*)


;; a default continuation for external calls to sicp evaluator
(define (done) #t)

(set! cont done)
(set! exp '(define id (lambda (x) x)))
(base-eval)

(set! exp '(define rcons (lambda (x y) (cons y x))))
(base-eval)

(set! exp '(define fib (lambda (n) (if (= n 1) 1 (if (= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))))
(base-eval)

(set! exp '(define fac (lambda (n) (if (= n 1) 1 (* n (fac (- n 1)))))))
(base-eval)

(set! exp '(define one-p (lambda (n) (if (= n 1) #t #f))))
(base-eval)

(set! exp '(define var-n (lambda (n) (= n 1))))
(base-eval)

;; (set! exp (macro-expand '(define map
;; 			   (letrec ((all-cars (lambda (xs)
;; 						(cond
;; 						 ((null? xs) xs)
;; 						 (else (cons (car (car xs))
;; 							     (all-cars (cdr xs)))))))
;; 				    (all-cdrs (lambda (xs)
;; 						(cond
;; 						 ((null? xs) xs)
;; 						 (else (cons (cdr (car xs))
;; 							     (all-cdrs (cdr xs)))))))
;; 				    (mymap3 (lambda (f xs)
;; 					      (cond
;; 					       ((null? xs) '())
;; 					       ((null? (car xs)) '())
;; 					       (else 
;; 						;;(newline)
;; 						(display "xs3 = ")
;; 						(display xs)
;; 						(newline)
;; 						(cons (apply f (all-cars xs))
;; 						      (mymap3 f (all-cdrs xs)))))))

;; 				    (mymap2 (lambda (f . xs)
;; 					      ;;(newline)
;; 					      (display "xs = ")
;; 					      (display xs)
;; 					      (newline)
;; 					      (cond
;; 					       ((null? xs) '())
;; 					       ((null? (car xs)) '())                      
;; 					       (else                        
;; 						(mymap3 f xs))))))
;; 			     mymap2))))
;; (base-eval)








