

;; when

;; define the macro initially as a function
;; then run the function over the let form we want to convert
;; assume not nested let otherwise we need an expression walker
;;


      
;; in defining let-rewrite we would want to use let also , interesting.
;; has to have atleast 3 parts
;; WHEN keyword , condition , body 
(define (when-rewrite exp)
  (let ((condition (cadr exp))
	(body (cddr exp)))
    (list (quote if)
	  condition
	  (cons (quote begin) body)
	  (quote #f))))

;; ;; delay evaluation by wrapping exp in a lambda

;; ;; ------ here what it looks like without fancy quasiquote
;; ;; ------ we can simulate let expression by naming another function
;; ;; -----  calling that instead which gives us new bindings
;; (define (when-rewrite exp)
;;   (let ((args (cadr exp))
;; 	(body (cddr exp)))
;;     (klet-rewrite2 args body)))

;; (define (klet-rewrite2 args body)
;;   (append (list (append (list (quote lambda))
;; 			(list (map car args))
;; 			body))
;; 	  (map (lambda (x) (cons (quote begin) (cdr x))) args)))
;; ;; ------- 
  
;; (klet-rewrite '(klet ((a 1)
;; 		    (b 2)
;; 		    (c 3))
;; 		(list a b c)))


(when-rewrite '(when #t 1 2 3 4 5))




