;;; $Id: chap5f.scm,v 4.3 2005/07/20 09:07:43 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;; everything goes forward and never returns
;; symbol looks little silly

;; rather than use genuinely unique symbol , we use kXXX eg k1 k2 k3 k4 to
;; represent continuations for readability


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

(define (lambda? x) #f)
;; check its a pair before take car x , so it doesnt trip up
(define (quote? x) (and (pair? x) (eq? (car x) 'quote)))

(define (cps exp kont)
  (display "exp = ") (display exp) (newline)
   (cond
    ((atom? exp)      (cps-atom exp kont))
    ((and (pair? exp) (quote? (car exp)))  (cps-quote exp kont))
    ((and (pair? exp) (lambda? (car exp))) (cps-lambda exp kont))
    ((and (pair? exp) (eq? (car exp) '+)) (cps-add exp kont))
    ((and (pair? exp) (eq? (car exp) '-)) (cps-minus exp kont))        
    ((and (pair? exp) (eq? (car exp) '*)) (cps-mult exp kont))
    ((and (pair? exp) (eq? (car exp) '>)) (cps-more exp kont))
    ((and (pair? exp) (eq? (car exp) '<)) (cps-less exp kont))        
    ((and (pair? exp) (eq? (car exp) '=)) (cps-eqv? exp kont))        
    ((and (pair? exp) (eq? (car exp) 'fact)) (cps-fact exp kont))        
    ((and (pair? exp) (eq? (car exp) 'if)) (cps-if exp kont))        
    ((pair? exp) (cps-app exp kont))
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

(define-syntax cps-binary
  (syntax-rules ()
      ((cps-binary op kont)
       (cps (cadr exp) `(lambda (,k1)
			  ,(cps (caddr exp)
				`(lambda (,k2)
				   (op ,k1 ,k2 ,kont))))))))


;; (cadr '(+ 1 2))  => 1
;; (caddr '(+ 1 2)) => 2
(define (k+ a b k)  (k (+ a b)))
(define (cps-add exp kont)
  (let ((k1 (gensym))
  	(k2 (gensym)))
    (cps (cadr exp) `(lambda (,k1)
		       ,(cps (caddr exp)
			     `(lambda (,k2)
				(k+ ,k1 ,k2 ,kont)))))))

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


(define (cps-fact exp kont)
  (let ((k1 (gensym)))
    (cps (cadr exp) `(lambda (,k1) (fact-k ,k1 ,kont)))))





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



;; typical cps version of factorial
(define fact-k (lambda (n k)
		 ((lambda (k19)
		    ((lambda (k18)
		       (k=
			k19
			k18
			(lambda (k12)
			  (if k12
			      ((lambda (k11) (id k11)) 1)
			      ((lambda (k14)
				 ((lambda (k17)
				    ((lambda (k16)
				       (k-
					k17
					k16
					(lambda (k15)
					  (fact-k k15
						  (lambda (k13) (k* k14 k13
								    (lambda (k10)
								      (id k10))))))))
				     1))
				  n))
			       n)))))
		     0))
		  n)))




  

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

;;(define (cps-lambda exp)
;;  `(lambda ,exp ,cont))

;; evaluate the arguments on by one then 
(define (cps-app exp kont)
  (display "cps-app : exp = ") (display exp) (newline)  
  (cps-terms (cdr exp) kont))



;; (+ a b c)
;;
;;
(define (cps-terms exp kont)
  (display "cps-terms : exp = ") (display exp) (newline)  
  #f) ;;(map cps exp))

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
