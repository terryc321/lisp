;;
;; my attempt at CPS conversion
;; 
;;
;; implement call/cc in terms of CPS , pretty simple i guess.
;; exceptions
;; signal errors
;; signal warning conditions
;; fluid-let stuff.
;;

;; everything goes forward and never returns
;; symbol looks little silly

;; rather than use genuinely unique symbol , we use kXXX eg k1 k2 k3 k4 to
;; represent continuations for readability

(define wrong error)

(define identity (lambda (x) x))
(define id identity)



(define gensym
  (let ((count 0))
    (lambda ()
      (let ((symbol (string->symbol (string-append "k" (number->string count)))))
	(set! count (+ count 1))
	symbol))))

;(define gensym generate-uninterned-symbol)


(define (atom? x) (not (pair? x)))

;; check its a pair before take car x , so it doesnt trip up
(define (quote? x) (and (pair? x) (eq? (car x) 'quote)))


(define (cps exp kont)
  (display "CPS : exp = ") (display exp) (newline)
   (cond
    ((atom? exp)      (cps-atom exp kont))

    ((and (pair? exp) (eq? (car exp) 'lambda)) (cps-lambda exp kont))    
    ((and (pair? exp) (eq? (car exp) 'if)) (cps-if exp kont))        
    ((and (pair? exp) (eq? (car exp) 'define)) (cps-define exp kont))
    ((and (pair? exp) (eq? (car exp) 'quote)) (cps-quote exp kont))

    ;; missing implicit begin sequence
    ;; missing set!
    ;; let
    ;; let*
    ;; letrec
    ;; call/cc 
    
    ;((and (pair? exp) (eq? (car exp) '+)) (cps-add exp kont))
    ;((and (pair? exp) (eq? (car exp) '-)) (cps-minus exp kont))        
    ;((and (pair? exp) (eq? (car exp) '*)) (cps-mult exp kont))
    ;((and (pair? exp) (eq? (car exp) '>)) (cps-more exp kont))
    ;((and (pair? exp) (eq? (car exp) '<)) (cps-less exp kont))
    ;((and (pair? exp) (eq? (car exp) '=)) (cps-eqv? exp kont))
    ;((and (pair? exp) (eq? (car exp) 'fact)) (cps-fact exp kont))
    ((and (pair? exp) (= (length exp) 2)) (cps-app-1 exp kont))
    ((and (pair? exp) (= (length exp) 3)) (cps-app-2 exp kont))
    ;; extend with variable argument applications
    
    (else "dunno")))






(define (cps-atom exp kont)
  (display "cps-atom : exp = ") (display exp) (newline)
  `(,kont ,exp))

    ;;`(,kont ,exp))
;;  `(,cont ,exp))


(define (cps-quote exp kont)
  (display "cps-quote : exp = ") (display exp) (newline)
  exp) ;;`(,kont ',(cadr exp)))
;;  `(cont ,(cadr exp)))

;; 
;; (+ a b)  kont
;; ((lambda (k1) ...) cps-a)
;;      ((lambda (k2) ...) cps-b)
;;            (k+ k1 k2 kont)

;; (define-syntax cps-binary
;;   (syntax-rules ()
;;       ((cps-binary op kont)
;;        (cps (cadr exp) `(lambda (,k1)
;; 			  ,(cps (caddr exp)
;; 				`(lambda (,k2)
;; 				   (op ,k1 ,k2 ,kont))))))))


;; (+ a b)  : (k+ a b kont)
;; (- a b)  : (k- a b kont)
;; (* a b)  : (k* a b kont)
;; (/ a b)  : (k/ a b kont)
;; (< a b)  : (k< a b kont)
;; (> a b)  : (k> a b kont)
;; (= a b)  : (k= a b kont) 


(define (cps-fact exp kont)
  (let ((k1 (gensym)))
    (cps (cadr exp) `(lambda (,k1) (fact-k ,k1 ,kont)))))


;; not yet evaluating f in (f 1 2)
;; is f a symbol ?
;; is f a macro ?
;; is f a compound procedure ?
;; are we evaluating f in (f 1 2 )
;; f
(define (cps-app-1 exp kont)
  (let ((k1 (gensym))
	(kop (string->symbol (string-append "k" (symbol->string (car exp))))))
    (cps (cadr exp) `(lambda (,k1) (,kop ,k1 ,kont)))))


(define (cps-app-2 exp kont)
  (let ((k1 (gensym))
  	(k2 (gensym))
	(kop (string->symbol (string-append "k" (symbol->string (car exp))))))
    (cps (cadr exp) `(lambda (,k1)
		       ,(cps (caddr exp)
			     `(lambda (,k2)
				(,kop ,k1 ,k2 ,kont)))))))


;; (cadr '(+ 1 2))  => 1
;; (caddr '(+ 1 2)) => 2
(define (k+ a b k)  (k (+ a b)))
(define (cps-add exp kont) (cps-app-2 exp kont))

  ;; (let ((k1 (gensym))
  ;; 	(k2 (gensym)))
  ;;   (cps (cadr exp) `(lambda (,k1)
  ;; 		       ,(cps (caddr exp)
  ;; 			     `(lambda (,k2)
  ;; 				(k+ ,k1 ,k2 ,kont)))))))

(define (k- a b k) (k (- a b)))
(define (cps-minus exp kont)
  (let ((k1 (gensym))
  	(k2 (gensym)))
    (cps (cadr exp) `(lambda (,k1)
		       ,(cps (caddr exp)
			     `(lambda (,k2)
				(k- ,k1 ,k2 ,kont)))))))


(define (k* a b k) (k (* a b)))
(define (cps-mult exp kont)
  (let ((k1 (gensym))
  	(k2 (gensym)))
    (cps (cadr exp) `(lambda (,k1)
		       ,(cps (caddr exp)
			     `(lambda (,k2)
				(k* ,k1 ,k2 ,kont)))))))



(define (k> a b k) (k (> a b)))
(define (cps-more exp kont)
  (let ((k1 (gensym))
  	(k2 (gensym)))
    (cps (cadr exp) `(lambda (,k1)
		       ,(cps (caddr exp)
			     `(lambda (,k2)
				(k> ,k1 ,k2 ,kont)))))))


(define (k< a b k) (k (< a b)))
(define (cps-less exp kont)
  (let ((k1 (gensym))
  	(k2 (gensym)))
    (cps (cadr exp) `(lambda (,k1)
		       ,(cps (caddr exp)
			     `(lambda (,k2)
				(k< ,k1 ,k2 ,kont)))))))


(define (k= a b k) (k (= a b)))
(define (cps-eqv? exp kont)
  (let ((k1 (gensym))
  	(k2 (gensym)))
    (cps (cadr exp) `(lambda (,k1)
		       ,(cps (caddr exp)
			     `(lambda (,k2)
				(k= ,k1 ,k2 ,kont)))))))


;; if want to cps factorial
(define fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))




(define (cps-if exp kont)
  (let ((k1 (gensym))
  	(k2 (gensym))
	(k3 (gensym)))
    (cps (cadr exp) `(lambda (,k1)
		       (if ,k1
			   ,(cps (caddr exp)
				 `(lambda (,k2) (,kont ,k2)))
			   ,(cps (cadddr exp)
				 `(lambda (,k3) (,kont ,k3))))))))


;; generate new continuation symbol represent where result of lambda will go
;; (lambda arg ...)
;; (lambda (args) ...)
(define (cps-lambda exp kont)
  (cond
   ((not (symbol? (cadr exp)))
    (display "CPS-LAMBDA : exp = ") (display exp) (newline)
    (let ((k1 (gensym)))
      `(lambda ,(append (cadr exp) (list k1))
	 ,(cps (caddr exp) k1))))
   (else
    (wrong "CPS-LAMBDA dont know how to handle this expression lambda one ARG." exp kont))))









;; define is a toplevel expression
;; no implicit begin YET.
;; only handles
;;     (define (fact n) ... )
;; rather than
;;     (define fact (lambda (n) ... ))
;; ASSUMES there is an implicit BEGIN on the ... in the lambda 
(define (cps-define exp kont)
  (cond
   ((pair? (cadr exp)) ;; (define (f....) )
    (let ((transform `(define ,(caadr exp) (lambda ,(cdadr exp) ,@(cddr exp)))))
      (display "cps-define: original =") (display exp) (newline)
      (display "cps-define: transform =") (display transform) (newline)      
      (cps-define transform kont)))
   ((symbol? (cadr exp))
    (let ((kop (string->symbol (string-append "k" (symbol->string (cadr exp))))))
      `(define ,kop ,(cps (caddr exp) kont))))
   (else (wrong 'cps-define exp kont))))






;; (define (cps-define exp kont)
;;   (cond
;;    ((pair? (cadr exp)) ;; (define (f....) )
;;     (let ((k1 (gensym)))
;;       `(define ,(append (cadr exp) (list k1))
;; 	 ,(cps (caddr exp) k1))))
;;    (else (wrong 'cps-define exp kont))))



;; typical cps version of factorial
;; we can cps the if construct using continuation k
;; 
(pretty-print (cps '(define (fact n) (if (= n 1) 1 (* n (fact (- n 1))))) 'k))

(define kfact
  (lambda (n k11)
    ((lambda (k21)
       ((lambda (k20)
          (k=
           k21
           k20
           (lambda (k14)
             (if k14
                 ((lambda (k13) (k11 k13)) 1)
                 ((lambda (k16)
                    ((lambda (k19)
                       ((lambda (k18)
                          (k-
                           k19
                           k18
                           (lambda (k17)
                             (kfact k17 (lambda (k15) (k* k16 k15 (lambda (k12) (k11 k12))))))))
                        1))
                     n))
                  n)))))
        1))
     n)))


(define fib (lambda (n) (if (= n 0) 0  (if (= n 1) 1  (+ (fib (- n 1)) (fib (- n 2)))))))

;; toplevel define fib ... make their own kontinuation
;; but we provide a dummy kontinuation k , for good measure.
(pretty-print (cps '(define fib (lambda (n) (if (= n 0) 0  (if (= n 1) 1  (+ (fib (- n 1)) (fib (- n 2))))))) 'k))

(define kfib
  (lambda (n k30)
    ((lambda (k48)
       ((lambda (k47)
          (k=
           k48
           k47
           (lambda (k33)
             (if k33
                 ((lambda (k32) (k30 k32)) 0)
                 ((lambda (k46)
                    ((lambda (k45)
                       (k=
                        k46
                        k45
                        (lambda (k36)
                          (if k36
                              ((lambda (k35) ((lambda (k31) (k30 k31)) k35)) 1)
                              ((lambda (k44)
                                 ((lambda (k43)
                                    (k-
                                     k44
                                     k43
                                     (lambda (k42)
                                       (kfib
                                        k42
                                        (lambda (k38)
                                          ((lambda (k41)
                                             ((lambda (k40)
                                                (k-
                                                 k41
                                                 k40
                                                 (lambda (k39)
                                                   (kfib
                                                    k39
                                                    (lambda (k37)
                                                      (k+
                                                       k38
                                                       k37
                                                       (lambda (k34)
                                                         ((lambda (k31) (k30 k31)) k34))))))))
                                              2))
                                           n))))))
                                  1))
                               n)))))
                     1))
                  n)))))
        0))
     n)))



(define check-fib
  (let ((values '(1 2 3 4 5 6 7 8 9 10)))
    (map (lambda (x y) (= x y))
	 (map (lambda (n) (kfib n id)) values)
	 (map (lambda (n) (fib n)) values))))

(define check-fac
  (let ((values '(1 2 3 4 5 6 7 8 9 10)))
    (map (lambda (x y) (= x y))
	 (map (lambda (n) (kfact n id)) values)
	 (map (lambda (n) (fact n)) values))))



(define checks (map (lambda (xs) (filter not xs)) (list check-fib check-fac)))

(kfact 10 id) ;; 3628800
(kfib 10 id)  ;; 55

(newline)
(newline)
(pretty-print "CPS.SCM was loaded.")
(newline)









    ;; `((lambda (,k1)
    ;; 	((lambda (,k2)
    ;; 	   (k+ ,k1 ,k2 ,kont)) ,b)) ,a)))
  
;;   (display "cps-add : exp = ") (display exp) (newline)
;;   (cond
;;    ((null? (cdr exp)) `(,kont 0)) ;; (+) = 0
;;    (else (cps-add-terms exp #f kont))))


;; (define (cps-add-terms exp r kont)
;;   (display "cps-add-terms : exp = ") (display exp) (newline)
;;   (display "cps-add-terms : r = ") (display r) (newline)
;;   (cond
;;    ((null? (cdr exp)) `(,kont r))
;;    (else (let ((k1 (gensym)))
;; 	   `((lambda (,k1) ,(cps-add-terms (cdr exp) k1 kont))
;; 	     ,(cps (car exp) #f))))))


;; ;; evaluate the arguments on by one then 
;; (define (cps-app exp kont)
;;   (display "cps-app : exp = ") (display exp) (newline)  
;;   (cps-terms (cdr exp) kont))



;; (+ a b c)
;;
;;
;; (define (cps-terms exp kont)
;;   (display "cps-terms : exp = ") (display exp) (newline)  
;;   #f) ;;(map cps exp))

  ;; (cond
  ;;  ((null? exp)
  ;;   (let ((k (gensym)))
  ;;     `(lambda (,k) (cont ,k))))
  ;;  (else (let ((k (gensym)))
  ;; 	   `(lambda (,k) ,(cps (car exp))
  ;; 		    (cps-terms (cdr exp)))))))





  

;; (let ((k (gensym "k")))
;;   (list k )))
;;    `(lambda (,k) (,k `(quote ,data)))))

;; (define (cps exp)    
;;   (if (pair? exp)
;;       (case (car e)
;;         ((quote)  (cps-quote e))
;;         ((if)     (cps-if (cadr e) (caddr e) (cadddr e)))
;;         ((begin)  (cps-begin (cdr e)))
;;         ((set!)   (cps-set! (cadr e) (caddr e)))
;;         ((lambda) (cps-abstraction (cadr e) (caddr e)))
;;         (else     (cps-application e)))
;;       (lambda (k) (k `,e))))


;; (define (cps-set! variable form)
;;   (lambda (k)
;;     ((cps form)
;;      (lambda (a)
;;        (k `(set! ,variable ,a)) ) ) ) ) 

;; (define (cps-if bool form1 form2)
;;   (lambda (k)
;;     ((cps bool)
;;      (lambda (b)
;;        `(if ,b ,((cps form1) k) 
;;                ,((cps form2) k))))))

;; (define (cps-begin e)
;;   (if (pair? e)
;;       (if (pair? (cdr e))
;;           (let ((void (gensym "void")))
;;             (lambda (k)
;;               ((cps (car e))
;;                (lambda (a)
;;                  ((cps-begin (cdr e))
;;                   (lambda (b)
;;                     (k `((lambda (,void) ,b) ,a)) ) ) ) ) ) )
;;           (cps (car e)) )
;;       (cps '()) ) )

;; (define (cps-application e)
;;   (lambda (k)
;;     (if (memq (car e) primitives)
;;         ((cps-terms (cdr e))
;;          (lambda (t*)
;;            (k `(,(car e) ,@t*)) ) )
;;         ((cps-terms e)
;;          (lambda (t*)
;;            (let ((d (gensym)))
;;              `(,(car t*) (lambda (,d) ,(k d)) 
;;                          . ,(cdr t*) ) ) ) ) ) ) )

;; (define primitives '( cons car cdr list * + - = pair? eq? ))

;; (define (cps-terms e*)
;;   (if (pair? e*)
;;       (lambda (k)
;;         ((cps (car e*))
;;          (lambda (a)
;;            ((cps-terms (cdr e*))
;;             (lambda (a*) 
;;               (k (cons a a*)) ) ) ) ) )
;;       (lambda (k) (k '())) ) )

;; (define (cps-abstraction variables body)
;;   (lambda (k)
;;     (k (let ((c (gensym "cont")))
;;             `(lambda (,c . ,variables)
;;                ,((cps body)
;;                  (lambda (a) `(,c ,a)) ) ) )) ) ) 

;; ((cps '(set! fact (lambda (n)
;; 		   (if (= n 1) 1
;; 		       (* n (fact (- n 1)))))))
;;  identity)
    

;; ((cps '(begin 1 2))
;;      identity)

;;; end of chap5f.scm

