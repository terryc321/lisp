
;; c { const }     constant names
;; v { var }       identifier
;; w { tsvar }     time stamped identifier
;; m { mactok }    macro token
;; e { mstree }    macro expression
;; f { tsmstree }  time stamped macro expression
;; s { stree }     expression
;; t { tsstree }   time stamped expression

;; x in union of sets { const var mactok coretok }
;; y in union of sets { const tsvar mactok coretok }
;; z in union of sets { coretok tsstree }

;; syntax
;; s ::= c | v | e | (lambda v s) | (s1 s2) , s1 NOT in set { mactok }
;; e ::= m | (m s1 ... sn) for n >= 0
;; t ::= c | v | w | f | (lambda v t) | (t1 t2) , t1 NOT in set { mactok }
;; f ::= m | (m t1 .. tn) for n >= 0 with above restriction

;; syntactic domains
;; STF = tsmstree -> tsstree
;; theta in set { ST } = tsmstree -> tsstree , < lambda term >

(define constant?
  (lambda (x)
    (or
     (boolean? x)
     (integer? x))))

(define macro-token?
  (lambda (v)
    (if 
     (and (symbol? v) (memq v '(LET
				IF
				OR
				NAIVE-OR
				FAKE
				CASE)))
     #t
     #f)))


(define macro?
  (lambda (m)
    (if (pair? m)
	(let ((op (car m)))
	  (cond
	   ((eq? op 'LET) #t)
	   ((eq? op 'IF) #t)
	   ((eq? op 'OR) #t)
	   ((eq? op 'NAIVE-OR) #t)
	   ((eq? op 'FAKE) #t)
	   ((eq? op 'CASE) #t)
	   (else #f)))
	#f)))




(define core-token?
  (lambda (v)
    (if (and (symbol? v) (memq v '(LAMBDA QUOTE)))
	#t
	#f)))

;; varialble is just a symbol 
(define variable?
  (lambda (x) (symbol? x)))


(define quote?
  (lambda (s)
    (and (pair? s)
	 (eq? (car s) 'QUOTE)
	 (pair? (cdr s))
	 (null? (cdr (cdr s))))))




;; ;; tsvar
;; ;; symbol with property original-name
;; (define tsvar?
;;   (lambda (x)
;;     #f))

;; (define tsstree?
;;   (lambda (x)
;;     #f))





;; S :: N -> var -> tsvar
;; S i v = v : i

;; make new datatype use a vector and a unique hidden marker that
;; cannot be forged 

;; i can still fake an s-tag by pulling it out
;; i --> n
;;

;; ------ property lists for symbols ------
;; once we register something as being macro token it stays , cos its only consing

;; -------- original ------------------
(define *originals* '())

(define (put-original-name new v)
  (set! *originals* (cons
		     (cons new v)
		     *originals*)))

(define (get-original-name new)
  (let ((v (assq new *originals*)))
    (if v
	(cdr v)
	#f)))

;; ------- mactok property -----------------
(define *macro-tokens* '())

(define (put-macro-token v)
  (set! *macro-tokens* (cons
		     (cons v #t)
		     *macro-tokens*)))

(define (get-macro-token s)
  (let ((v (assq s *macro-tokens*)))
    (if v
	(cdr v)
	#f)))


;; ------- coretok property -----------------
(define *core-tokens* '())

(define (put-core-token v)
  (set! *core-tokens* (cons
		     (cons v #t)
		     *core-tokens*)))

(define (get-core-token s)
  (let ((v (assq s *core-tokens*)))
    (if v
	(cdr v)
	#f)))

;;-------------------------------------------




;; 
;; time stamp variable
(define stamp
  (lambda (n)
    (let ((seen '()))
      (lambda (v)
	(let ((info (assq v seen)))
	  (if info
	      (cdr info)
	      (let ((new (make-symbol (symbol->string v))))
		(put-original-name new v)
		
		(set! seen
		  (cons
		   (cons v new)
		   seen))
		new)))))))
		

(define S stamp)
(define S0 (S 0))


;;      0    1  2   3
;; #[ stamp var n unique ]
(define stamped?
  (lambda (x)
    (and (symbol? x)
	 (if (get-original-name x)
	     #t
	     #f))))
  
;; */* :: tsvar x var -> tsstree -> tsstree
;; where tsstrees are restricted to time stamped lambda terms


;; T : Time Stamp
;; function T parses the stree and stamps all identifier leaves
;; with the function tau.
;; the initial pass tau is the function S0 = (S 0).
;; Then the real expansion process begins and the clock value
;; is increased to 1.
;;                           stamped
;; y in union of sets { const tsvar mactok coretok }
;; T[y] = \tau . y
;; v in { variable }
;; T[v] = \tau . tau v
(define T
  (lambda (s)
    (lambda (tau)
      (cond
       ((constant? s) s) ;; const
       ((stamped? s)  s) ;; tsvar
       ((macro-token?  s)  s) ;; mactok
       ((core-token? s)  s) ;; coretok
       ((variable? s) (tau s)) ;; T[v] = tau v
       ((pair? s) (map (lambda (z) ((T z) tau)) s))
       (else (error " T cannot handle this form " s tau ))))))




;; fun
;; arg

;; var
(define lambda-variable
  (lambda (s)
    (if (lambda? s)
	(begin
	  (car (cdr s)))
	(error "lambda-variable expected lambda form " s))))

;; body
(define lambda-body
  (lambda (s)
    (if (lambda? s)
	(begin
	  (car (cdr (cdr s))))
	(error "lambda-body expected lambda form " s))))

;; add1
(define add1  (lambda (x) (+ x 1)))

;; (lambda VAR
(define lambda?
  (lambda (s)
    (and (pair? s)
	 (eq? 'LAMBDA (car s))
	 (pair? (cdr s))
	 (variable? (car (cdr s)))
	 (pair? (cdr (cdr s)))
	 (null? (cdr (cdr (cdr s)))))))


;; theta

;; application?
;; (F ARG)
(define application?
  (lambda (s)
    (and (pair? s)
	 (pair? (cdr s))
	 (null? (cdr (cdr s))))))


;; fun
;; (F ARG) -> F
(define application-function 
  (lambda (s)
    (car s)))

;; arg
;; (F ARG) -> ARG
(define application-argument
  (lambda (s)
    (car (cdr s))))


;; E : Expand
(define E
  (lambda (s)
    (lambda (theta)
      (lambda (j)
	(cond
	 ((constant? s) s)
	 ((stamped? s) s)
	 ((quote? s) s)
	 ((macro? s)
	  (((E ((T (theta s)) (S j))) theta)
	   (add1 j)))
	 ((lambda? s)
	  `(lambda ,(lambda-variable s)
	     ,(((E (lambda-body s)) theta) j)))
	 ((application? s)
	  `(,(((E (application-function s)) theta) j)
	    ,(((E (application-argument s)) theta) j))))))))









