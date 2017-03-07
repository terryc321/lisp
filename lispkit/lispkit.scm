
;; Lispkit lisp taken from peter henderson book functional programming secd machine
(define-module (lispkit)
  #:export (secd exec result compil locate comp exprs vars complis
		 instr-count))




;; do something
;; (define-syntax for/until
;;   (syntax-rules (until do)
;;     ((for/until i until lim do ...)
;;      (do ((i 1 (+ i 1)))
;; 	 ((>= i lim) #f)
;;        ...))))

(define-syntax for/until
  (syntax-rules ()
    ((for/until i init lim . body)
     (do ((i init (+ i 1)))
	 ((>= i lim) #f)
       (begin . body)))))

(define-syntax exec/m
  (syntax-rules ()
    ((_ fun args)
     (exec 'fun 'args))))


;; #t
;; #f
;; #nil
(define nil '())

(define s nil)
(define e nil)
(define c nil)
(define d nil)
(define w nil)
(define result nil)


(define (range a b)
  (cond
   ((>= a b) '())
   (else (cons a (range (+ a 1) b)))))


(define instr-count
  (let ((syms '(ld-nil ld ldc ldf ap rtn dum rap sel join scar scdr atom scons eq
		       add sub mul div rem leq stop null rcons)))
    (let ((statistics (map (lambda (s) (cons s 0)) syms)))
      (lambda (sym)
	(cond
	 ((eq? sym 'display)
	  (format #t "~%*********** secd statistics ******************~%")
	  ;;(format #t " syms = ~a ~% statistics = ~a ~%" syms statistics)
	  (map (lambda (s i) (format #t "~a : ~a ~%" s (cdr i))) syms statistics)
	  (format #t   "**********************************************~%")	  
	  #f)
	 ((eq? sym 'reset)
	  (set! statistics (map (lambda (x) (cons s 0))  syms))
	  #f)
	 (else ;; side effect
	  ;;(format #t "statistics = ~a ~%" statistics)	
	  (let ((ass (assoc sym statistics)))
	    ;;(format #t " ~a ~% sym = ~a ~%" ass sym)
	    (if ass
		(begin
		  (assoc-set! statistics sym (+ 1 (cdr (assoc sym statistics))))
		  #f)
		(begin
		  (error "symbol not known : instr-count ~a ~%" sym))))))))))





;; single step
(define (single-step)
  (let ((op (car c)))
    (cond
     ((eq? op 'nil) (ld-nil)) ;; 0
     ((eq? op 'ld) (ld))      ;; 1
     ((eq? op 'ldc) (ldc))    ;; 2
     ((eq? op 'ldf) (ldf))    ;; 3
     ((eq? op 'ap) (ap))      ;; 4
     ((eq? op 'rtn) (rtn))    ;; 5
     ((eq? op 'dum) (dum))    ;; 6
     ((eq? op 'rap) (rap))    ;; 7
     ((eq? op 'sel) (sel))    ;; 8
     ((eq? op 'join) (join))  ;; 9     
     ((eq? op 'car) (scar))   ;; 10
     ((eq? op 'cdr) (scdr))   ;; 11
     ((eq? op 'atom) (atom))  ;; 12
     ((eq? op 'cons) (scons)) ;; 13
     ((eq? op 'eq) (eq))      ;; 14     
     ((eq? op 'add) (add))    ;; 15
     ((eq? op 'sub) (sub))    ;; 16
     ((eq? op 'mul) (mul))    ;; 17
     ((eq? op 'div) (div))    ;; 18
     ((eq? op 'rem) (rem))    ;; 19
     ((eq? op 'leq) (leq))    ;; 20
     ((eq? op 'stop) #f)      ;; 21     
     ((eq? op 'null) (null))  ;; 22
     ((eq? op 'rcons) (rcons)) ;; 23     
     (else (error "secd error OPERATOR unknown: CYCLE" op)))))



(define (cycle)
  (let ((op (car c)))
    (if (eq? op 'stop)
        (begin
	  (set! result (car s))
	  result)
	(begin
	  (single-step)
	  (cycle)))))


;;(define (exec fn args)
(define (exec fn args)
  (set! s (cons args '()))
  (set! e '())
  (set! c fn)
  (set! d '())
  (cycle))


(define (secd s1 e1 c1 d1)
  (set! s s1)
  (set! e e1)
  (set! c c1)
  (set! d d1)
  (cycle))


;; load nil
(define (ld-nil)
  (instr-count 'ld-nil)
  ;;
  (set! s (cons '() s))
  (set! c (cdr c)))

     
;; load constant
;; ( ldc ? <code> )
(define (ldc)
  (instr-count 'ldc)
  ;;
  (set! s (cons (car (cdr c)) s))
  (set! c (cdr (cdr c))))



;; load variable from environment
(define (ld)
  (instr-count 'ld)
  ;;
  (set! w e)
  ;;(format #t "environment = ~a ~% b . n = ~a " e (car (cdr c)))
  ;; for i 1 until car(car(cdr c)) do
  ;;     w = cdr w
  (for/until i 1 (car (car (cdr c)))
	     (set! w (cdr w)))
  (set! w (car w))
  ;; for i 1 until cdr(car(cdr c)) do
  ;;     w = cdr w
  (for/until i 1 (cdr (car (cdr c)))
	     (set! w (cdr w)))
  (set! w (car w))
  (set! s (cons w s))
  (set! c (cdr(cdr c))))





(define (null)
  (instr-count 'null)
  ;;
  (cond
   ((null? (car s))
    (set! s (cons #t (cdr s))))
   (else
    (set! s (cons #f (cdr s)))))
  (set! c (cdr c)))


(define (atom)
  (instr-count 'atom)
  ;;
  (cond
   ((number? (car s))
    (set! s (cons #t (cdr s))))
   ((symbol? (car s))
    (set! s (cons #t (cdr s))))
   (else
    (set! s (cons #f (cdr s)))))
  (set! c (cdr c)))


;; RCONS
(define (rcons)
  (instr-count 'rcons)
  ;;
  (set! s (cons (cons 		 
		 (car s)
		 (car (cdr s)))
		(cdr (cdr s))))
  (set! c (cdr c)))


;; SCONS
(define (scons)
  (instr-count 'scons)
  ;;
  (set! s (cons (cons 
		 (car (cdr s))
		 (car s))
		(cdr (cdr s))))
  (set! c (cdr c)))

;; CAR
(define (scar)
  (instr-count 'scar)
  ;;
  (set! s (cons (car (car s))
		(cdr s)))
  (set! c (cdr c)))

;; CDR
(define (scdr)
  (instr-count 'scdr)
  ;;
  (set! s (cons (cdr (car s))
		(cdr s)))
  (set! c (cdr c)))





;; arithmetic

(define (sub)
  (instr-count 'sub)
  ;;
  (set! s (cons (- (car (cdr s))
		   (car s))
		(cdr (cdr s))))
  (set! c (cdr c)))


(define (add)
  (instr-count 'add)  
  ;;
  (set! s (cons (+ (car (cdr s))
		   (car s))
		(cdr (cdr s))))
  (set! c (cdr c)))

  
(define (mul)
  (instr-count 'mul)  
  ;;
  (set! s (cons (* (car (cdr s))
		   (car s))
		(cdr (cdr s))))
  (set! c (cdr c)))


(define (div)
  (instr-count 'div)
  ;;
  (set! s (cons (/ (car (cdr s))
		   (car s))
		(cdr (cdr s))))
  (set! c (cdr c)))


(define (rem)
  (instr-count 'rem)  
  ;;  
  (set! s (cons (remainder (car (cdr s))
		   (car s))
		(cdr (cdr s))))
  (set! c (cdr c)))


(define (leq)
  (instr-count 'leq)  
  ;;
  (if (<= (car (cdr s))
	  (car s))
      (begin
	(set! s (cons #t (cdr (cdr s)))))
      (begin
	(set! s (cons #f (cdr (cdr s))))))
  (set! c (cdr c)))



(define (eq)
  (instr-count 'eq)  
  ;;
  (if (or (and (symbol? (car s))
	       (symbol? (car (cdr s)))
	       (eq? (car s) (car (cdr s))))
	  (and (number? (car s))
	       (number? (car (cdr s)))
	       (= (car s) (car (cdr s)))))
      (begin 
	(set! s (cons #t (cdr (cdr s)))))
      (begin
	(set! s (cons #f (cdr (cdr s))))))
  (set! c (cdr c)))





(define (ldf)
  (instr-count 'ldf)  
  ;;
  (set! s (cons (cons (car (cdr c)) e)
		s))
  (set! c (cdr (cdr c))))






(define (ap)
  (instr-count 'ap)  
  ;;
  (set! d (cons (cdr (cdr s))
		(cons e (cons (cdr c) d))))
  (set! e (cons (car (cdr s))
		(cdr (car s))))
  (set! c (car (car s)))
  (set! s '()))


(define (rtn)
  (instr-count 'rtn)  
  ;;
  (set! s (cons (car s)
		(car d)))
  (set! e (car (cdr d)))
  (set! c (car (cdr (cdr d))))
  (set! d (cdr (cdr (cdr d)))))



(define (dum)
  (instr-count 'dum)  
  ;;
  (set! e (cons '() e))
  (set! c (cdr c)))




(define (rap)
  (instr-count 'rap)
  ;;
  (set! d (cons (cdr (cdr s))
		(cons (cdr e)
		      (cons (cdr c)
			    d))))
  (set! e (cdr (car s)))
  (set-car! e (car (cdr s)))
  (set! c (car (car s)))
  (set! s '()))



(define (sel)
  (instr-count 'sel)
  ;;
  (set! d (cons (cdr (cdr (cdr c))) d))
  (if (car s)
      (begin
	(set! c (car (cdr c))))
      (begin
	(set! c (car (cdr (cdr c))))))
  (set! s (cdr s)))


(define (join)
  (instr-count 'join)
  ;;
  (set! c (car d))
  (set! d (cdr d)))


;; compil-er

(define (compil e)
  (comp e '() '(stop)))

(define (atom? e)
  (not (pair? e)))

(define (comp e n c)
  (cond
   ;; number
   ((number? e) 
    (cons 'ldc (cons e c)))
   ;; boolean
   ((boolean? e) 
    (cons 'ldc (cons e c)))
   ;; symbol?
   ((symbol? e)
    (cons 'ld (cons (locate e n) c)))
   ;; quote
   ((eq? (car e) 'quote)
    (cons 'ldc (cons (car (cdr e)) c)))
   ;; add
   ((eq? (car e) 'add)
    (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons 'add c))))
   ;; sub
   ((eq? (car e) 'sub)
    (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons 'sub c))))
   ;; mul
   ((eq? (car e) 'mul)
    (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons 'mul c))))
   ;; div
   ((eq? (car e) 'div)
    (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons 'div c))))
   ;; rem
   ((eq? (car e) 'rem)
    (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons 'rem c))))
   ;; leq
   ((eq? (car e) 'leq)
    (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons 'leq c))))
   ;; eq 
   ((eq? (car e) 'eq)
    (comp (car (cdr e)) n (comp (car (cdr (cdr e))) n (cons 'eq c))))
   ;; car
   ((eq? (car e) 'car)
    (comp (car (cdr e)) n (cons 'car c)))
   ;; cdr 
   ((eq? (car e) 'cdr)
    (comp (car (cdr e)) n (cons 'cdr c)))
   ;; atom
   ((eq? (car e) 'atom)
    (comp (car (cdr e)) n (cons 'atom c)))
   ;; null
   ((eq? (car e) 'null)
    (comp (car (cdr e)) n (cons 'null c)))
   
   ;; cons
   ((eq? (car e) 'cons)
    (comp (car (cdr e)) n
	  (comp (car (cdr (cdr e))) n (cons 'cons c))))
   
   ;; if
   ((eq? (car e) 'if)
    (let ((thenpt (comp (car (cdr (cdr e))) n '(join)))
	  (elsept (comp (car (cdr (cdr (cdr e)))) n '(join))))
      (comp (car (cdr e)) n (cons 'sel (cons thenpt (cons elsept c))))))
   ;; lambda - let - letrec need to be rewritten
   
   ;; lambd-a
   ((eq? (car e) 'lambda)
    (let ((body (comp (car (cdr (cdr e))) (cons (car (cdr e)) n) '(rtn))))
      (cons 'ldf (cons body c))))

   ;; let 
   ((eq? (car e) 'let)
    (let ((m (cons (vars (car (cdr e))) n))
	  (args (exprs (car (cdr e)))))
      (let ((body (comp (car (cdr (cdr e))) m '(rtn))))
	(complis args n (cons 'ldf (cons body (cons 'ap c)))))))
      
   ;; letrec
   ((eq? (car e) 'letrec)
    (let ((m (cons (vars (car (cdr e))) n))
	  (args (exprs (car (cdr e)))))
      (let ((body (comp (car (cdr (cdr e))) m '(rtn))))
	(cons 'dum
	      (complis args m
		       (cons 'ldf (cons body (cons 'rap c))))))))
   ;; otherwise
   (else (complis (cdr e) n (comp (car e) n (cons 'ap c))))))



;; (define (complis e n c)
;;   (cond
;;    ((null? e) (cons 'ldc (cons '() c)))
;;    (else (complis (cdr e) n (comp (car e) n (cons 'rcons c))))))

(define (complis e n c)
  (cond
   ((null? e) (cons 'ldc (cons '() c)))
   (else (comp (car e) n (complis (cdr e) n (cons 'cons c))))))


(define (incar el)
  (cons (+ 1 (car el)) (cdr el)))


(define (posn e n)
  (if (eq? e (car n))
      1
      (+ 1 (posn e (cdr n)))))


(define (member e n)
  (cond
   ((null? n) #f)
   ((eq? e (car n)) #t)
   (else (member e (cdr n)))))



;; environment
(define (locate e n)
  (if (member e (car n))
      (begin
	(cons 1 (posn e (car n))))
      (begin
	(incar (locate e (cdr n))))))



;; takes all 1st items of pairs
;; ((a . 1)(b . 2)(c . 3)(d . 4)) --> (a b c d)
;; map car d
(define (vars d)
  (if (null? d)
      '()
      (cons (car (car d))
	    (vars (cdr d)))))


;; takes all 2nd
;; ((a . 1)(b . 2)(c . 3)(d . 4)) --> (1 2 3 4)
;; map cdr d
(define (exprs d)
  (if (null? d)
      '()
      (cons (car (cdr (car d)))
	    (exprs (cdr d)))))


