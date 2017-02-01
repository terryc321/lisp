

;; klet
;; let in core syntax
;; apply cps conversion to get klet

;; define the macro initially as a function
;; then run the function over the let form we want to convert
;; assume not nested let otherwise we need an expression walker
;;


;; "LOADED let-star.scm -- nothing defined yet tho. "

;; (let* ((a 1)
;;       (b 2)
;;       (c 3))
;;   (list a b c))

;; ((lambda (a) ((lambda (b)  ((lambda (c) (list a b c)) (+ b 1))) (+ a 1))) 1)
;; ((lambda (a) ((lambda (b)  ((lambda (c) (list a b c)) 3))2)) 1)

;; implicit begin on the variable arguments
;; implicit begin in the body of let-star
      
;; in defining let-rewrite we would want to use let also , interesting.
;; has to have atleast 3 parts
;; LET keyword , ARGs , BODY 
(define (klet-star-rewrite exp)
  (klet-star-rewrite2 (cadr exp) (cddr exp)))

(define (klet-star-rewrite2 args body)
  (cond
   ((null? args) (cons (quote begin) body))
   (else
    (list
     (append (list (quote lambda)
		   (list (caar args)))	   
	     (list (klet-star-rewrite2 (cdr args) body)))
     (cons (quote begin) (cdar args))))))

(klet-star-rewrite '(klet* ((a 1)
		    (b 2)
		    (c 3))
		(list a b c)))

(klet-star-rewrite '(klet* ((a 1 2 3)
			    (b 4 5 6)
			    (c 7 8 9))
			   (list a b c)
			   (list a b c)
			   (list c b a)))





