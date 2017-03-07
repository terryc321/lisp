

;;--------------------------------------------------------------------------
;; some common macros -- please load macro-expander before using this file
;; 
;; as long the lisp reader can deal with quasiquote unquote unquote-splicing
;;--------------------------------------------------------------------------





;; quote expander
;; dont think quote is a lambda expression is it ??
;;(install-macro 'quote  (lambda (expr) expr))


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

;;-----------------------------------------------------------------------
