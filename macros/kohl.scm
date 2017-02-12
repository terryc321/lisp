
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

;;----------------------------------------------------------------------
;; add core forms -- eg IF
;; need to add how to expand that form onto E 
;; need to add it to core-token?
;; need to remove it from macro-token?
;; need to add phrase into */*
;; need to add phrase into A
;;









(define constant?
  (lambda (x)
    (or
     (boolean? x)
     (integer? x))))

(define macro-token?
  (lambda (v)
    (if 
     (and (symbol? v) (memq v '(LET			     
				OR
				NAIVE-OR
				FAKE
				CASE)))
     #t
     #f)))


;;
;; if it is a CORE-TOKEN then its not a MACRO-TOKEN
;;
(define macro?
  (lambda (m)
    (if (pair? m)
	(let ((op (car m)))
	  (cond
	   ((eq? op 'LET) #t)
	   ;;((eq? op 'IF) #t)
	   ((eq? op 'OR) #t)
	   ((eq? op 'NAIVE-OR) #t)
	   ((eq? op 'FAKE) #t)
	   ((eq? op 'CASE) #t)
	   (else #f)))
	#f)))





;;
;; extend core primitives IF and SET! by adding them here.
;;  
(define core-token?
  (lambda (v)
    (if (and (symbol? v) (memq v '(LAMBDA
				   QUOTE
				   IF
				   SET!
				   )))
	#t
	#f)))



;; variable is just a symbol
;; atomic non variable
;; not stamped
;; not macro-token
;; not core-token
(define variable?
  (lambda (x)
    (symbol? x)))



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
	      (let ((new (gensym (format #f "~a:~a:" v n))))
	      ;;(let ((new (string->symbol (format #f "~a:~a" v n))))
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


;; theta -- ST is the actual expander
(define ST
  (lambda (m)
    (cond
     ;; LET
     ((and (pair? m) (eq? (car m) 'LET))
      (let ((i (car (cdr m)))
	    (e (car (cdr (cdr m))))
	    (b (car (cdr (cdr (cdr m))))))
	`((LAMBDA ,i ,b) ,e)))
     
     ;; IF
     ;; ((and (pair? m) (eq? (car m) 'IF))
     ;;  (let ((a (car (cdr m)))
     ;; 	    (b (car (cdr (cdr m))))
     ;; 	    (c (car (cdr (cdr (cdr m))))))
     ;; 	`(((ef ,a) ,b) ,c)))
     
     ;; OR
     ((and (pair? m) (eq? (car m) 'OR))
      (let ((a (car (cdr m)))
	    (b (car (cdr (cdr m)))))
	`(LET v ,a (IF v v ,b))))
     ;; NAIVE-OR
     ((and (pair? m) (eq? (car m) 'NAIVE-OR))
      (let ((a (car (cdr m)))
	    (b (car (cdr (cdr m)))))
	(let ((v (S0 'v)))
	  `(LET ,v ,a (IF ,v ,v ,b)))))
     ;; FAKE
     ((and (pair? m) (eq? (car m) 'FAKE))
      (let ((x (car (cdr m))))
	`(QUOTE ,x)))
     ;; CASE exp pr
     ((and (pair? m) (eq? (car m) 'CASE))
      (let ((exp (car (cdr m)))
	    (pr  (car (cdr (cdr m)))))
	`(LET v ,exp
	      (IF ((eq? v)
		   (QUOTE ,(car pr)))
		  ,(car (cdr pr))
		  #f))))
     (else (error "syntax table: no match" m)))))



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



;; IF a b c
;; CDR= a b c
;; CDR= b c
;; CDR= c
;; CDR= ()
(define (if? s)
  (and (pair? s)
       (eq? 'IF (car s))
       (pair? (cdr s))
       (pair? (cdr (cdr s)))
       (pair? (cdr (cdr (cdr s))))
       (null? (cdr (cdr (cdr (cdr s)))))))

       
       
       

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
	  (((E ((T (theta s)) (S j))) theta) (add1 j)))
	 ((lambda? s)
	  `(LAMBDA ,(lambda-variable s)
		   ,(((E (lambda-body s)) theta) j)))
	 ((if? s)
	  `(IF ,(((E (car (cdr s))) theta) j)
	       ,(((E (car (cdr (cdr s)))) theta) j)
	       ,(((E (car (cdr (cdr (cdr s))))) theta) j)))	       
	 ((application? s)
	  `(,(((E (application-function s)) theta) j)
	    ,(((E (application-argument s)) theta) j)))
	 (else
	  (error "expand E : un matched " s theta j )))))))








;; atomic non variable
(define atomic-non-variable?
  (lambda (y)
    (or (constant? y)
	(stamped? y)
	(macro-token? y)
	(core-token? y))))



;; atomic-not-stamped?
(define atomic-not-stamped?
  (lambda (x)
    (or (constant? x)
	(and (variable? x)
	     (not (stamped? x)))
	(macro-token? x)
	(core-token? x))))


;; U
(define U
  (lambda (t)
    (format #t "U t = ~a ~%" t)
    (cond
     ((atomic-not-stamped? t) t)
     ((stamped? t)
      (get-original-name t))
     (else (map U t)))))




;; */*
(define */*
  (lambda (v w)
    (lambda (t)
      (format #t "*/* v w t  = ~a : ~a : ~a ~%" v w t)
      (cond
       ((stamped? t) (if (eq? t w) v t))
       ((atomic-not-stamped? t) t)
       ((quote? t) t)
       ((lambda? t)
	(if (eq? w (lambda-variable t))
	    `(LAMBDA ,w ,(lambda-body t))
	    `(LAMBDA ,(lambda-variable t)
		     ,((*/* v w)
		       (lambda-body t)))))
       ((if? t)
	`(IF ,((*/* v w) (car (cdr t)))
	     ,((*/* v w) (car (cdr (cdr t))))
	     ,((*/* v w) (car (cdr (cdr (cdr t)))))))
       
       ((application? t)
	`(,((*/* v w) (application-function t))
	  ,((*/* v w) (application-argument t))))))))





;; A
(define A
  (lambda (s)
    (format #t "A s  = ~a ~%" s)
    (cond
     ((variable? s) s)
     ((atomic-non-variable? s) s)  
     ((quote? s) s)
     ((lambda? s)
      (let ((v (gensym (format #f "~a:~a:"
			       (U (lambda-variable s))
			       "new"))))
	`(LAMBDA ,v
		 ,(A ((*/* v (lambda-variable s)) ;; */*
		      (lambda-body s))))))

     ((if? s)
      `(IF ,(A (car (cdr s)))
	   ,(A (car (cdr (cdr s))))
	   ,(A (car (cdr (cdr (cdr s)))))))
     
     ((application? s)
      `(,(A (application-function s))
	,(A (application-argument s)))))))




;; Ehyg
(define Ehyg
  (lambda (s)
    (lambda (theta)
      (U
       (A 
	(((E ((T s) S0)) theta) 1)
	)))
    ))








      
;;(U (A
;;;    )))))



(define (demo0) ((T '(a b c)) S0))
(define (demo1) ((T '(QUOTE x)) S0))
(define (demo2) (((E ((T '(QUOTE x)) S0)) ST) 1))
(define (demo3) (((E ((T '(IF a b c)) S0)) ST) 1))

(define (check1) ((Ehyg '(LET x (OR a v) (NAIVE-OR x v))) ST))
(define (check2) ((Ehyg '(LAMBDA a (CASE (FAKE a) (QUOTE a)))) ST))

(define (check3) ((Ehyg '(LET a 1 (IF a b c))) ST))
;;(define (check4) (((E ((T '(IF a b c)) S0)) ST) 1))


;; quasiquote -- doesnt require .
;; (define (check4) ((Ehyg '(LET a 1 `(a ,a a))) ST))




























