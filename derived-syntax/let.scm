

;; klet
;; let in core syntax
;; apply cps conversion to get klet

;; define the macro initially as a function
;; then run the function over the let form we want to convert
;; assume not nested let otherwise we need an expression walker
;;

;;"LOADED let.scm -- nothing defined yet tho. "

(let ((a 1)
      (b 2)
      (c 3))
  (list a b c))

((lambda (a b c) (list a b c)) 1 2 3) 
      
;; in defining let-rewrite we would want to use let also , interesting.
;; has to have atleast 3 parts
;; LET keyword , ARGs , BODY 
(define (klet-rewrite exp)
  (let ((args (cadr exp))
	(body (cddr exp)))
    `((lambda ,(map car args) ,@body) ,@(map (lambda (x) `(begin ,@(cdr x))) args))))

;; ------ here what it looks like without fancy quasiquote
;; ------ we can simulate let expression by naming another function
;; -----  calling that instead which gives us new bindings
(define (klet-rewrite exp)
  (let ((args (cadr exp))
	(body (cddr exp)))
    (klet-rewrite2 args body)))

(define (klet-rewrite2 args body)
  (append (list (append (list (quote lambda))
			(list (map car args))
			body))
	  (map (lambda (x) (cons (quote begin) (cdr x))) args)))
;; ------- 
  
(klet-rewrite '(klet ((a 1)
		    (b 2)
		    (c 3))
		(list a b c)))

(klet-rewrite '(klet ((a 1 2 3)
		    (b 4 5 6)
		    (c 7 8 9))
		     (list a b c)
		     (list 10 11 12)))






