
;;
;; because using poor mans closure objects
;; we can just message pass to object itself
;;
(define (tag-backquote? x)
  (and (pair? x)
       (pair? (cdr x))
       (or (eq? (car x) 'backquote)
	   (eq? (car x) 'quasiquote))))

(define (tag-comma? x)
  (and (pair? x)
       (pair? (cdr x))
       (or (eq? (car x) 'comma)
	   (eq? (car x) 'unquote))))

(define (tag-comma-atsign? x)
  (and (pair? x)
       (pair? (cdr x))
       (or (eq? (car x) 'comma-atsign)
	   (eq? (car x) 'unquote-splicing))))
	   

;; retrieve the boxed object from within backquote , comma and comma-atsign s
;; 2nd item of list
(define (tag-data x)
  (car (cdr x)))

;;
;; bawden expansion algorithm
;;

(define (qq-expand x)
  (cond ((tag-comma? x)
	 (tag-data x))
	((tag-comma-atsign? x)
	 (error "illegal"))
	((tag-backquote? x)
	 (qq-expand
	  (qq-expand (tag-data x))))
	((pair? x)
	 `(append
	   ,(qq-expand-list (car x))
	   ,(qq-expand (cdr x))))
	(else `',x)))

(define (qq-expand-list x)
  (cond ((tag-comma? x)
	 `(list ,(tag-data x)))
	((tag-comma-atsign? x)
	 (tag-data x))
	((tag-backquote? x)
	 (qq-expand-list
	  (qq-expand (tag-data x))))
	((pair? x)
	 `(list
	   (append
	    ,(qq-expand-list (car x))
	    ,(qq-expand (cdr x)))))
	(else `'(,x))))

;;
;; recursively explore structure.
;;


;; to expand a given quasi-quotation , strip off leading backquote
;; pass it to qq-expand for processing
(define (qq x)
  (cond
   ((tag-backquote? x)
    (qq-expand (tag-data x)))
   (else x)))



;;
;; ------------------------------------------------------------------------
;; simple a - list of macro expanders
;;
;; macro expander
;;
(define *macro-expanders* '())

(define (install-macro keyword procedure)
  (set! *macro-expanders* (cons (list keyword procedure)
				*macro-expanders*)))

(define (get-macro keyword)
  (assoc keyword *macro-expanders*))



(define (macro-expand expr)
  (display "macro expanding :")
  (display expr)
  (newline)
  (if (not (pair? expr))
      expr
      (let ((keyword (car expr)))
	(let ((expander (get-macro keyword)))
	  (if  (pair? expander)
	       (begin
		 (display "found macro expander : ")
		 (display expander)
		 (newline)
		 ;; test prevents infinitely calling macro expansion
		 (let ((original expr)
		       (new ((car (cdr expander)) expr)))
		   (if
		    (equal? original new)
		    new
		    (macro-expand ((car (cdr expander)) expr)))))
	       (begin		   
		 (map macro-expand expr)))))))


;; (define (macro-expand expr)
;;   (display "macro expanding :")
;;   (display expr)
;;   (newline)
;;   (if (not (pair? expr))
;;       expr
;;       (let ((keyword (car expr)))
;; 	(cond
;; 	 ;;((equal? keyword 'quote) expr)
;; 	 ;; ((equal? keyword 'lambda)
;; 	 ;;  (append
;; 	 ;;   (list 'lambda (cadr expr))
;; 	 ;;   (map macro-expand (cddr expr))))
;; 	 (else
;; 	  (let ((expander (get-macro keyword)))
;; 	    (if  (pair? expander)
;; 		 (begin
;; 		   (display "found macro expander : ")
;; 		   (display expander)
;; 		   (newline)
;; 		   (let ((original expr)
;; 			 (new ((car (cdr expander)) expr)))
;; 		     (if
;; 		      (equal? original new)
;; 		      new
;; 		      (macro-expand ((car (cdr expander)) expr)))))
;; 		 (begin		   
;; 		   (map macro-expand expr)))))))))

;;--------------------------------------------------------------------------
;; hide actual expansion implementation inside a letrec if required
;;--------------------------------------------------------------------------

;; quote expander
(install-macro 'quote  (lambda (expr) expr))

;; lamb-da expander
(install-macro 'lambda (lambda (expr)
		    (append (list 'lambda (cadr expr))
			    (map macro-expand (cddr expr)))))



;; install quasiquote expander
;;
(install-macro 'quasiquote qq)
(install-macro 'backquote qq)

;;    ((when condition body ...)
;;     (if condition (begin body ...) #f))))


;; when --> if
(install-macro
 'when
 (lambda (expr)
   `(if ,(car (cdr expr))
	(begin ,@(cddr expr))
	#f)))

;; different when macro that just expands its terms 
;; (install-macro 'when
;; 	       (lambda (expr)
;; 		 (append (list 'when)
;; 			 (map macro-expand (cdr expr)))))


;;
;; install COND macro
;; assuming a well formed cond macro ?
;; (cond (c1 e1a e1b e1c)(ELSE else1 else2 else3))
;; (if c1 (begin e1a e1b e1c) ...)
;;
;; install cond macro
(install-macro
 'cond
 (lambda (expr)
  ;; (cond ...
  (define (c expr)
    (c2 (cdr expr)))
  ;; ((c1 ...)(c2 ...)(else ...))
  (define (c2 expr)
    (let ((condition (car (car expr)))
	  (consequences (cdr (car expr))))
      (cond
       ((eq? condition 'else)
	(cons 'begin consequences))
       (else (list 'if
		   condition
		   (cons 'begin consequences)
		   (c2 (cdr expr)))))))
  (c expr)))



;; let macro
;; (let ((a 1 2 3)(b 2 3 4)(c 5 6 7)) ... )
;; ((lambda (a b c) ...) (begin 1 2 3)(begin 2 3 4)(begin 5 6 7))
(install-macro
 'let
 (lambda (expr)
   (append (list (append (list 'lambda)
			 (list (map car (cadr expr)))
			 (cddr expr)))  
	   (map (lambda (x) (cons 'begin (cdr x))) (cadr expr)))))

;; let*
;; (let* ((a 1 2 3)(b 4 5 6)(c 7 8 9)) ... )
;; ->
;; (let ((a 1 2 3)) (let ((b 4 5 6)) (let ((c 7 8 9)) ...)))
;; ->
;;
(install-macro
 'let*
 (lambda (expr)
   (define (c expr)
     (c2 (car (cdr expr))  (cddr expr)))

   (define (c2 varvals body)
     (cond
      ((null? varvals) (cons 'begin body))
      (else `(let (,(car varvals)) ,(c2 (cdr varvals) body)))))
   (c expr)))

;; EXAMPLE - REWRITE LET* IN TERMS OF LET
;; EXAMPLE - LET IN TERMS OF LAMBDA 

;; letrec
(install-macro
 'letrec
 (lambda (expr)
   (define (c expr)
     (c2 (car (cdr expr))  (cddr expr)))
   (define (c2 varvals body)
     (cond
      ((null? varvals) (cons 'begin body))
      (else (append
	     (list 'let)
	     (list (map (lambda (x) (list (car x) #f)) varvals))
	     (list (append (list 'begin)
			   (append
			    (map (lambda (x) (list 'set! (car x) (cons 'begin (cdr x)))) varvals)
			    body)))))))
   (c expr)))

;;
;; case
;;

;; ----------------


(macro-expand '(when 1 (when 2 `(1 2 3) 3 4 5 "job done")))
(macro-expand '(cond (1 2 3)(4 5 6)(7 8 9)(else (when 1 (when 2 `(1 2 3) 3 4 5 "job done")))))
(macro-expand '(let ((a 1 2 3)(b 4 5 6)(c 7 8 9)) (list a b c)(list c b a)(list b a c)))
(macro-expand '(let* ((a 1 2 3)(b 4 5 6)(c 7 8 9)) (list a b c)(list c b a)(list b a c)))

(pp (macro-expand '(letrec ((even?
			 (lambda (n)
			   (if (zero? n)
			       #t
			       (odd? (- n 1)))))
			(odd?
			 (lambda (n)
			   (if (zero? n)
			       #f
			       (even? (- n 1))))))
		     (even? 88))))

(pp (macro-expand '(letrec ((fac (lambda (n) (if (< n 2) n (* n (fac (- n 1))))))) (list (fac 5)(fac 10)))))



































  
  


  
