

;; (append)
;; (append '(1 2 3))
;; (append '(1 2 3) '(4 5 6))
;; (append '(1 2 3) '(4 5 6) '(7 8 9))
;; (append '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12))

;; (define (append . args)
;;     (letrec ((app2 (lambda (xs ys)
;; 		     (cond
;; 		      ((null? xs) ys)
;; 		      (else (cons (car xs)
;; 				  (app2 (cdr xs) ys))))))
;; 	     (app (lambda (xs ys)
;; 		    (cond		     
;; 		     ((null? xs) '()) ; no list		     
;; 		     ((null? (cdr xs)) (car xs)) ; 1st list only 
;; 		     (else (cons (car (car xs))
;; 				 (app (cdr (car xs)) (cdr xs))))))))
;;       (app args))))

(define (append . args)
  (letrec ((app2 (lambda (xs ys) ;; xs , ys both simple lists
		   (cond
		    ((null? ys) xs)
		    ((null? xs) ys)
		    (else (cons (car xs)
				(app2 (cdr xs) ys))))))   
	   
	   (app (lambda (xs lists-of-ys)
		  (cond
		   ((null? lists-of-ys) xs)
		   ((null? (cdr lists-of-ys)) (app2 xs (car lists-of-ys)))
		   (else (app (app2 xs (car lists-of-ys))
			      (cdr lists-of-ys)))))))
  
    (cond
     ((null? args) args)
     ((null? (cdr args)) (car args))
     (else (app (car args) (cdr args))))))


(display "append defined")
(newline)






