;		A Scheme -to- syntax-rule compiler.
;
; This code mechanically converts (desugared) pure-functional
; Scheme procedures to the corresponding CPS syntax-rule macros.
;
; This code implements an algorithm outlined in
; http://pobox.com/~oleg/ftp/Scheme/macros.html#Macro-CPS-programming
;
; A Bigloo Scheme system is assumed. We extensively rely on the
; match-case form provided by Bigloo.
;
; $Id: cps-macro-conv.scm,v 2.3 2003/09/03 20:35:45 oleg Exp oleg $

(module cps-macro-convert
	(include "myenv-bigloo.scm"))

; From SRFI-1
(define (fold-right kons knil lis1)
    (let recur ((lis lis1))
       (if (null? lis) knil
	    (let ((head (car lis)))
	      (kons head (recur (cdr lis)))))))

; Left fold combinator for a single list
(define (fold kons knil lis1)
  (let lp ((lis lis1) (ans knil))
    (if (null? lis) ans
      (lp (cdr lis) (kons (car lis) ans)))))

; Bigloo has these
;(define (symbol-append s1 s2)
;  (string->symbol (string-append (symbol->string s1) (symbol->string s2))))

; (define (filter pred lst)
;   (match-case lst
;    ((?h . ?t) (if (pred h) (cons h (filter pred t)) (filter pred t)))
;    (else '())))

;------------------------------------------------------------------------
;	An environment to store the meaning of identifiers
;
; An env is an assoc list, a list of lists:
;  (id cps-name key value ...)
; where id is an identifier, cps-name is the name for that id
; to use in the CPS code, and key value pairs specify optional properties.
; A key is a DSSSL keyword.
; Properties:
;   if-fork:
;      if id is the name of a predicate such as null? or memq,
;      the if-fork property, if given, tells the name of a cps-macro
;      that accepts two additional continuations: the true-branch and the
;      false branch. The forking macro passes the computed result of the
;      predicate to the correct branch.
;   cps-code:
;      the code to emit for the symbol
;   applies-directly:
;      #t if the code specifies a macro, which doesn't need ??!apply

; All the property keywords, for 'type checking'
(define all-property-keywords
  '(if-fork: cps-code: applies-directly:))

; A constructor
; (env-extend env id cps-name key value ...)
(define (env-extend env id cps-name . options)
  (assert (list? env))
  (assert (even? (length options)))
  ; make sure that any property keyword is in all-property-keywords
  (assert 
   (let loop ((options options))
     (or (null? options) (and (memq (car options) all-property-keywords)
			      (loop (cddr options))))))
  (cons
   (cons* id cps-name options)
   env))


; (lookup env id)
; Find the binding for id in env, which must exist
; return a list (cps-name key value ...)
(define (lookup env id)
  (cond
   ((assq id env) => cdr)
   (else (error "Unbound id: " id nl "in env: " env))))

; Lookup a cps-name for an id
(define (lookup-cps-name env id)
  (car (lookup env id)))

; Lookup a key parameter for an id. Return #f if not found
(define (lookup-key? env id key)
  (assert (memq key all-property-keywords)) ; a 'type' check
  (and-let* 
   ((options (cdr (lookup env id)))
    (option (memq key options)))
   (cadr option)))

; add options to an already defined name
(define (env-add-options env id . options)
  (assert (even? (length options)))
  (match-case (lookup env id)
    ((?cps-name . ?old-options)
     (apply env-extend (cons* env id cps-name (append options old-options))))
    (else (error "env-add-options: can't happen"))))

; Give the list of all ids in the env
; Note that env may have duplicate bindings
(define (env-all-ids env)
  (let loop ((env env) (acc '()))
    (match-case env
      (() acc)
      (((?h . ?-) . ?t) (loop t (if (memq h acc) acc (cons h acc)))))))

; Initial environment
(define init-env
  (fold
   (lambda (extender res) (extender res))
   '()
   `(
     ,(lambda (env)
	(env-extend env 'car '?car
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?car
	      (syntax-rules ()
		((_ (x . y) k) (??!apply k x))))))
     ,(lambda (env)
	(env-extend env 'cdr '?cdr
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?cdr
	      (syntax-rules ()
		((_ (x . y) k) (??!apply k y))))))
     ,(lambda (env)
	(env-extend env 'cons '?cons
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?cons
	      (syntax-rules ()
		((_ x y k) (??!apply k (x . y)))))))
     ,(lambda (env)
	(env-extend env 'null? '?null?
	   applies-directly: #t
	   if-fork: 'ifnull?
	   cps-code:
	   '(define-syntax ?null?
	      (syntax-rules ()
		((_ () k) (??!apply k #t))
		((_ x k)  (??!apply k #f))))))
     ,(lambda (env)
	(env-extend env 'ifnull? '?ifnull?
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?ifnull?
	      (syntax-rules ()
		((_ () kt kf) (??!apply kt #t))
		((_ x kt kf) (??!apply kf #f))))))
     ,(lambda (env)
	(env-extend env 'pair? '?pair?
	   applies-directly: #t
	   if-fork: 'ifpair?
	   cps-code:
	   '(define-syntax ?pair?
	      (syntax-rules ()
		((_ (a . b) k) (??!apply k #t))
		((_ not-pair k)  (??!apply k #f))))))
     ,(lambda (env)
	(env-extend env 'ifpair? '?ifpair?
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?ifpair?
	      (syntax-rules ()
		((_ (a . b) kt kf) (??!apply kt #t))
		((_ not-pair kt kf) (??!apply kf #f))))))
     ,(lambda (env)  ; this is just an identity. We need it for the forking
	(env-extend env 'true? '?true?  ; predicate
	   applies-directly: #t
	   if-fork: 'iftrue?
	   cps-code:
	   '(define-syntax ?true?
	      (syntax-rules ()
		((_ x k)  (??!apply k x))))))
     ,(lambda (env)
	(env-extend env 'iftrue? '?iftrue?
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?iftrue?
	      (syntax-rules ()
		((_ #f kt kf) (??!apply kf #f))
		((_ x kt kf) (??!apply kt #t))))))
     ,(lambda (env)
	(env-extend env 'append '?append
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?append
	      (syntax-rules ()
		((_ (x ...) (y ...) k) (??!apply k (x ... y ...)))))))
     ,(lambda (env)
	(env-extend env 'eq? '?eq?
	   applies-directly: #t
	   if-fork: 'ifeq?))
     ,(lambda (env)
	(env-extend env 'ifeq? '?ifeq?
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?ifeq?
	      (syntax-rules ()
		((_ (x . y) b kt kf) ; a is not a symbol: always false
		 (??!apply kf #f))
		((_ () b kt kf) ; a is not a symbol: always false
		 (??!apply kf #f))
		((_ a b _kt _kf)
		 (let-syntax
		     ((aux 
		       (syntax-rules (a)
			 ((_ a kt kf)
			  (??!apply kt #t))
			 ((_ other kt kf)
			  (??!apply kf #f)))))
		   (aux b _kt _kf)))))
	   ))
     ,(lambda (env)
	(env-extend env 'memq '?memq
	   applies-directly: #t
	   if-fork: 'ifmemq?))
     ,(lambda (env)
	(env-extend env 'ifmemq? '?ifmemq?
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?ifmemq?
	      (syntax-rules ()
		((_ a lst kt kf)
		 (?ifpair? lst
		    (??!lambda (_) ; it's a pair
		       (?car lst
			  (??!lambda (x)
			     (?ifeq? a (??! x)
				    ; match
				  (??!lambda (_) (??!apply kt #t))
				    ; mismatch
				  (??!lambda (_)
				     (?cdr lst
					(??!lambda (tail)
					   (?ifmemq? a (??! tail) kt kf))))))))
		    (??!lambda (_)
			       (??!apply kf #f))))))
	   ))

     ; For the Eratosthenes sieve
     ; Church-Peano representation for integers
     ,(lambda (env)
	(env-extend env 'number-zero '?number-zero
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?number-zero
	      (syntax-rules ()
		((_ k) (??!apply k ()))))))

     ,(lambda (env)
	(env-extend env 'number-two '?number-two
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?number-two
	      (syntax-rules ()
		((_ k) (??!apply k ((())) ))))))


     ,(lambda (env)
	(env-extend env 'incr '?incr
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?incr
	      (syntax-rules ()
		((_ n k) (??!apply k  (n) ))))))

     ,(lambda (env)
	(env-extend env 'decr '?decr
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?decr
	      (syntax-rules ()
		((_ (n) k) (??!apply k  n ))))))


     ,(lambda (env)
	(env-extend env 'less-than-two? '?less-than-two?
	   applies-directly: #t
	   if-fork: 'ifless-than-two?
	   cps-code:
	   '(define-syntax ?less-than-two?
	      (syntax-rules ()
		((_ ((n)) k) (??!apply k #f))
		((_ x k)  (??!apply k #t))))))
     ,(lambda (env)
	(env-extend env 'ifless-than-two? '?ifless-than-two?
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?ifless-than-two?
	      (syntax-rules ()
		((_ ((n)) kt kf) (??!apply kf #f))
		((_ x kt kf) (??!apply kt #t))))))

     ,(lambda (env)
	(env-extend env 'number-zero? '?number-zero?
	   applies-directly: #t
	   if-fork: 'ifnumber-zero?
	   cps-code:
	   '(define-syntax ?number-zero?
	      (syntax-rules ()
		((_ () k) (??!apply k #t))
		((_ x k)  (??!apply k #f))))))
     ,(lambda (env)
	(env-extend env 'ifnumber-zero? '?ifnumber-zero?
	   applies-directly: #t
	   cps-code:
	   '(define-syntax ?ifnumber-zero?
	      (syntax-rules ()
		((_ () kt kf) (??!apply kt #t))
		((_ x kt kf) (??!apply kf #f))))))

     )))


;------------------------------------------------------------------------
;		Macro-CPS (MCPS) conversion
;
; The man conversion routine accepts a list of (define (proc ...) body)
; forms and the initial envioronment. The defined procedures, along
; with the standard ones, must form a self-contained graph.
;
;
; Limitations:
; - only fixed-arity lambdas are supported. If the variable number of arguments
; is needed, pack the arguments into a list yourself
; - no quasiquote. Use cons explicitly
; - all bindings in a letrec expression must be to lambda-forms.


; generating variable names
(define (kgensym) (symbol-append 'k (gensym)))
(define (make-pattern-var x) (symbol-append '_? x))
(define (make-cps-macro-name x) (symbol-append '? x))

; dealing with values
(define (literal? x)
  (match-case x
    ((quote ?-) #t)
    ((? pair?) #f)
    (()
     (error "bad literal: " x))
    (else #t)))

(define (value? x)
  (or (literal? x)
      (match-case x
       ((lambda . ?-) #t)
       (else #f))))
    
(define (all-values? lst) (or (null? lst) (and (value? (car lst))
						(all-values? (cdr lst)))))

; MCPS of a value: the Psi function of Fisher's CPS-transform
(define (conv-value env val)
  (match-case val
    ((quote ?x) x) ; quote is not needed in a macro, drop it
    ((lambda ?args ?body)
     (convert-lambda env args body))
    ((? symbol?) (lookup-cps-name env val))
    ((or (? boolean?) (? number?)) val) ; self-evaluating
    (else
     (error "bad value: " val))))

;		Constructors of CPS expressions

; convert an exp into cps and wrap into a macro-lambda whose
; argument is irrelevant
; Our ??!lambda needs at least one argument always
(define (make-dummy-macro-lambda env exp k)
  (let ((x (gensym)))
    ; don't store x in the env: it's dummy anyway
    `(??!lambda (,x) ,(convert-exp env exp k))))

; convert an exp into cps and wrap into a macro-lambda.
; lexp is a proc such as (lexp formal-arg) should generate
; the expression to convert
(define (make-macro-lambda env lexp k)
  (let* ((x (gensym))
	 (env (env-extend env x `(??! ,x))))
    `(??!lambda (,x) ,(convert-exp env (lexp x) k))))

; Generate a macro application
; we assume that all arguments in a list args are simple values.
; fn can be either a symbol or a macro-abstraction.
(define (make-macro-app env fn args . ks)
  (let* ((args (map (lambda (arg) (conv-value env arg)) args))
	 (app (cons 
	       (if (symbol? fn) (lookup-cps-name env fn) fn)
	       (append args ks))))
    (cond
     ((and (symbol? fn) (lookup-key? env fn applies-directly:))
      app)
     (else (cons '??!apply app)))))


; The top-level procedure: convert the nest of defines, which define
; a set of mutually-recursive procedures.
; Return the extended env.
(define (convert-defines env defines)

  ; first, pre-scan all the defines and determine the names of the symbols
  ; they bind. Add these symbols to the env.
  ; the code will be generated later
  (let*
      ((env
	(fold
	 (lambda (define-form env)
	   (match-case define-form
	     ((define ((and (? symbol?) ?name) . ?-) . ?-)
	      (env-extend env name
			  (make-cps-macro-name name)
			  applies-directly: #t))
	     (else
	      (error "Invalid define form in the list of defines: "
		     define-form))))
	 env
	 defines))

       (cerr "stage1 of convert-defines done" nl)
    ; Now, for each (define (name . args) body)
    ; CPS-convert the body and generate
    ;      (define-syntax cps-name
    ;        (syntax-rules ()
    ; 	 ((_ pattern-args k) conv-body)))
       (env
	(fold
	 (lambda (define-form env)
	   (match-case define-form
	     ((define (?name . ?args) ?body)
	      (let* ((cps-name (lookup-cps-name env name))
		     (k (make-pattern-var (kgensym)))
		     (int-env (env-extend env k k))
		     ; bind all args in the env
		     (int-env
		      (fold
		       (lambda (arg int-env)
			 (env-extend int-env arg (make-pattern-var arg)))
		       int-env args))
		     (conv-body
		      (convert-exp int-env body k))
		     )
		(env-add-options env name
		    cps-code:
		    `(define-syntax ,cps-name
		       (syntax-rules ()
			 ((_ ,@(map
				(lambda (arg) (lookup-cps-name int-env arg))
				args)
			     ,k)
			  ,conv-body))))))
	     (else
	      (error "Invalid define form in the list of defines: "
		     define-form))))
	 env
	 defines))
       )
    env))


; Convert a letrec nest. All bindings must be of the form
;   (name (lambda (arg1 arg2 ...) lambda-body))
; Processing of letrec is very similar to that of defines.
; We have to scan the bindings twice: once to add the names to the env
; (so the lambda-body can refer to the names). We generate the code
; on the seconds pass.
;  (letrec ((name (lambda (arg1 arg2 ...) lambda-body))) body)
; becomes
;  (letrec-syntax
;     ((?name (syntax-rules ()
;            ((_ ?arg1 ?arg2 ... ?k) conv-lambda-body)))
;    conv-body)
(define (convert-letrec env bindings body k)

  ; first, pre-scan all the defines and determine the names of the symbols
  ; they bind. Add these symbols to the env.
  ; the code will be generated later
  (let*
      ((env
	(fold
	 (lambda (binding env)
	   (match-case binding
	     (((and (? symbol?) ?name) ?-)
	      (env-extend env name
			  (make-cps-macro-name name)
			  applies-directly: #t))
	     (else
	      (error "Invalid binding form: "
		     binding))))
	 env
	 bindings))

       (cerr "stage1 of convert-letrec done" nl)
    ; Now, for each (name (lambda args body))
    ; CPS-convert the body and generate a letrec-syntax binding
    ;      (name (syntax-rules ()
    ; 	 ((_ pattern-args k) conv-body)))
       (conv-bindings
	(map
	 (lambda (binding)
	   (match-case binding
	     ((?name (lambda ?args ?body))
	      (let* ((cps-name (lookup-cps-name env name))
		     (k (make-pattern-var (kgensym)))
		     (int-env (env-extend env k k))
		     ; bind all args in the env
		     (int-env
		      (fold ; we must make sure that the names of pattern vars
		            ; are unique. pattern vars can't be shadowed
		       (lambda (arg int-env)
			 (env-extend int-env arg
				     (make-pattern-var 
				      (symbol-append arg (gensym)))))
		       int-env args))
		     (conv-body
		      (convert-exp int-env body k))
		     )
		`(,cps-name (syntax-rules ()
			 ((_ ,@(map
				(lambda (arg) (lookup-cps-name int-env arg))
				args)
			     ,k)
			  ,conv-body)))))
	     (else
	      (error "Invalid binding form in letrec: "
		     binding))))
	 bindings))
       )
    `(letrec-syntax ,conv-bindings
       ,(convert-exp env body k))
    ))

; Convert an expression
; The basic CPS algorithm:
; 
; i1 (Vrs var) k = (At k (Vrt var))
; i1 (As t1 t2) k = let v1 = (fresh "m1_" k)
;                       v2 = (fresh "m2_" k)
; 		  in (i1 t1) $
;                      (Lt v1 (i1 t2 $ (Lt v2 (At (At (Vrt v1) (Vrt v2)) k))))
; i1 (Ls var body) k = let k1 = fresh "k1_" k
; 		     in (At k (Lt var (Lt k1 (i1 body (Vrt k1)))))

(define (convert-exp env exp k)
  (define (if-forking? x) (lookup-key? env x if-fork:))

  (cerr "convert-exp: " exp nl)

  (match-case exp

   ; convert cond into ifs
   ((cond (else . ?stmt))
    (convert-exp env stmt k))
   ((cond (?pred ?true-br) (else ?false-br))
    (convert-exp env `(if ,pred ,true-br ,false-br) k))
   ((cond (?pred ?br) . ?other-br)
    (convert-exp env `(if ,pred ,br (cond . ,other-br)) k))
   ((cond . ?-)
    (error "bad cond: " exp))

   ; convert an if stmt: (if pred true-br false-br)
   ; both branches must be present
   ; If pred has a form (standard-pred args ...)
   ; we convert the form into
   ; (standard-pred-cps-fork args ... k-true-br k-false-br)
   ((if ((and (? if-forking?) ?forking-pred) .
	      (and (? all-values?) ?args))
	?true-br ?false-br)
    (let ((forking-symb (lookup-key? env forking-pred if-fork:)))
      (assert forking-pred)
      (make-macro-app env forking-symb args
	(make-dummy-macro-lambda env true-br k)
	(make-dummy-macro-lambda env false-br k))))
   ((if '#f ?true-br ?false-br)  ; #f doesn't need a quotation, but tolerales
    (convert-exp env false-br k))
   ((if '?- ?true-br ?false-br)  ; quoted literal is considered true
    (convert-exp env true-br k))
   ; complex predicate
   ; (if (pred args) true-br false-br) ==>
   ; (conv (pred args) \x.(if x true-br false-br))
   ((if (?pred . ?args) ?true-br ?false-br) ; complex predicate
    (convert-exp env `(,pred . ,args)
       (make-macro-lambda env (lambda (x) `(if ,x ,true-br ,false-br)) k)))
   ; (if var true-br false-br) => fork
   ((if (and (? symbol?) ?var) ?true-br ?false-br) ; true? is a forking pred
    (convert-exp env `(if (true? ,var) ,true-br ,false-br) k))
   ((if #f ?true-br ?false-br)
    (convert-exp env false-br k))
   ((if ?- ?true-br ?false-br)
    (convert-exp env true-br k))
   ((if . ?-)
    (error "unknown if: " exp))

;   ((?- . ?-) exp)

   ; convert a letrec nest
   ((letrec ?bindings ?body)
    (convert-letrec env bindings body k))

   ; primitive expressions
   ((? value?) (make-macro-app env k (list exp)))

   ; Convert an application. Try to use a bit of finesse and
   ; avoid too many administrative redexes
        ; a non-value is applied. Introduce apply-mv to
	; eliminate the non-value from the head position
        ; This rule also shifts the lambda form into the arg position
   (((?x . ?y) . ?z)
    (convert-exp env `(apply-mv (,x . ,y) . ,z)))
        ; things like (0 x) are always in error
   (((not (? symbol?)) . ?-)
    (error "Invalid head of the application: " exp))
	; a value is applied to other values: no adm redexes needed
        ; (fn v1 ...vn) => (fn-cps v1 ... vn k)
   ((?fn . (and (? all-values?) ?args))
    (make-macro-app env fn args k))
        ; (fn nv) => ((conv nv) (\x. (fn x k)))
   ((?fn ?arg)
    (convert-exp env arg
      (make-macro-lambda env (lambda (x) `(,fn ,x)) k)))
        ; (fn nv1 v2 nv3) =>
        ; (conv nv1 (\v1 (conv nv3 \v3 (fn-cps v1 v2 v3 k))))
   ((?fn . ?args)
    (let*
	((complex-args (filter (lambda (x) (not (value? x))) args))
	 (symbs (map (lambda (_) (gensym)) complex-args))
	 (env (fold
	       (lambda (x env)
		 (env-extend env x `(??! ,x)))
	       env symbs))
	 (simple-args
	  (let loop ((args args) (symbs symbs))
	    (cond
	     ((null? args) '())
	     ((value? (car args))
	      (cons (car args) (loop (cdr args) symbs)))
	     (else
	      (cons (car symbs) (loop (cdr args) (cdr symbs)))))))
	 (cps-app (make-macro-app env fn simple-args k))
	 )
      (assert (pair? complex-args))
      (let loop ((complex-args (reverse complex-args))
		 (symbs (reverse symbs))
		 (k cps-app))
	(if (null? complex-args) k
	    (loop (cdr complex-args) (cdr symbs)
		  (convert-exp env (car complex-args)
		    `(??!lambda (,(car symbs)) ,k)))))))

   ((?- . ?-)
    (error "bad application: " exp))


   (else
    (cerr "can't convert yet: " exp nl)
    exp)
))


; i1 (Ls var body) k = let k1 = fresh "k1_" k
; 		     in (At k (Lt var (Lt k1 (i1 body (Vrt k1)))))

(define (convert-lambda env args body)
  (let* ((k (kgensym))
	 (int-env (env-extend env k `(??! ,k)))
		     ; bind all args in the env
	 (int-env
	  (fold
	   (lambda (arg int-env)
	     (env-extend int-env arg `(??! ,arg)))
	   int-env args))
	 (conv-body
	  (convert-exp int-env body (lookup-cps-name int-env k)))
	 )
    `(??!lambda ,(append args (list k)) ,conv-body)))

;------------------------------------------------------------------------
;			Testing
; Deep reverse function

(define test-reverse*
 '((define (reverse* lst)
  (cond
   ((null? lst) lst)
   ((pair? lst) (append (reverse* (cdr lst))
			(cons (reverse* (car lst)) '())))
   (else lst)))
   )
)

; Write out all generated code, along with the initial env and the
; code for ??!apply
; Prepend the test code as well, so we could run the result as a regression
; test.
(define (write-out init-env source-code test-code)
  (let ((env
	 (convert-defines init-env source-code)))
    (cout nl "---- the env after the convertion" nl)
    (pp env)
    (with-output-to-file "/tmp/a.scm"
      (lambda ()
        ; first write out the definition of ??!apply
        ; the first S-expression from the file below
	(write
	 (with-input-from-file "macro-lambda.scm" read))
	(newline)
	(for-each
	 (lambda (id)
	   (and-let* ((code (lookup-key? env id cps-code:)))
		     (write code) (newline)))
	 (env-all-ids env))
	(write test-code)
	(newline)
	(write '(newline))
    )
)))


(define test-map
  '((define (map f lst)
      (if (pair? lst)
	  (cons (f (car lst))
		(map f (cdr lst)))
	  lst))))

; (define test-quotify
;   `(
;     ,@test-map
;     (define (quotify-fn symbs-l tree)
;       (define (doit tree)
; 	(map 
; 	 (lambda (node)
; 	   (if (pair? node)
; 					; recurse to process children
; 	       (doit node)
; 	       (if (memq node symbs-l)
; 					; replace the leaf
; 		   (cons 'unquote (cons node '()))
; 		   node)))
; 	 tree))
;       (cons 'quasiquote (cons (doit tree) '())))
; ))

(define test-quotify1
  (append
   test-map
   '((define (quotify symbs-l tree)
      (letrec 
	  ((doit
	    (lambda (tree)
	      (map 
	       (lambda (node)
		 (if (pair? node)
					; recurse to process children
		     (doit node)
		     (if (memq node symbs-l)
					; replace the leaf
			 (cons 'unquote (cons node '()))
			 node)))
	       tree))))
	(cons 'quasiquote (cons (doit tree) '())))))
))

(define test-Eratosthenes
  '(
    (define (iota n)
      (letrec
	  ((loop 
	    (lambda (curr counter)
	      (if (less-than-two? counter) '()
		  (cons curr (loop (incr curr) (decr counter)))))))
	(loop (number-two) n)))
    (define (sieve lst)
      (letrec
	  ((choose-pivot ; choose the first non-zero element in lst
	    (lambda (lst)
	      (cond
	       ((null? lst) lst)
	       ((number-zero? (car lst)) ; skip over zeros
		(cons (car lst) (choose-pivot (cdr lst))))
	       (else 
		(cons (car lst)          ; sieve with the pivot, and recurse
		      (choose-pivot
		       (do-sieve (car lst) (decr (car lst)) (cdr lst))))))))
	   ; Run the sieve
	   ; (do-sieve step current lst)
	   ; Return the new lst with each step-th element sieved out:
	   ; replaced by 0.
	   (do-sieve
	(lambda (step current lst)
	  (cond
	   ((null? lst) lst)
	   ((number-zero? current)    ; replace the current element with zero
	    (cons (number-zero) (do-sieve step (decr step) (cdr lst))))
	   (else
	    (cons (car lst) (do-sieve step (decr current) (cdr lst)))))))
	   )
	(choose-pivot lst)))
    (define (is-prime n)
      (if (number-zero? (car (reverse (sieve (iota n)))))
	  'composite
	  'prime))
    (define (reverse lst)
      (letrec ((loop
		(lambda (lst accum)
		  (if (null? lst) accum
		      (loop (cdr lst) (cons (car lst) accum))))))
	(loop lst '())))
))

; (display (quotify-fn '(i j k) '(i4 k i5 l () (m i) ((('i))))))

; (write-out init-env test-reverse*
; 	   '(display
; 	     (?reverse* (1 (2 3) (4 (5)) 6 () 7)
; 			(??!lambda (x) (display '(??! x))))))

; (write-out init-env test-map
; 	   '(?map
; 	     (??!lambda (x k)
; 			(??!apply (??! k) (+ 1 (??! x))))
; 	     (1 2 3 4)
; 	     (??!lambda (x) (display '(??! x))))
; )

; (write-out init-env test-quotify1
; ;	   '(?quotify (i j k) (s4 i)
; ;	   '(?quotify (i j k) (s4 k s5 l () (m i) ((('i))))
; 		      (??!lambda (r) (display '(??! r))))
; )

; (write-out init-env test-quotify1
; 	   '(let ((i 'symbol-i) (j "str-j") (k "str-k"))
; 	      (display (?quotify (i j k) (s4 k ('i))
; ;	      (display (?quotify (i j k) (s4 k ((('i))))
; ;	      (display (?quotify (i j k) (s4 k s5 l () (m i) ((('i))))
; 				 (??!lambda (r) (begin (??! r))))))
; )

; (write-out init-env test-Erathosthenes
; 	   '(?iota ((((( () )))))
; 		    (??!lambda (x) (display '(??! x))))
; )

; (write-out init-env test-Erathosthenes
; 	   '(?iota ((((((( () )))))))
; 	       (??!lambda (x) (?sieve (??! x)
; 		    (??!lambda (x) (display '(??! x))))))
; )

(write-out init-env test-Eratosthenes
	   '(?is-prime ((((( () )))))
;	   '(?is-prime (((((( () ))))))
		    (??!lambda (x) (display '(??! x))))
)
