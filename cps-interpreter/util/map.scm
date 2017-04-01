
;; ---------------------- map --------------------------------------------


;; surprisingly difficult to get this right , really.
;; firstly map takes any number of arguments - so need dotted pairs on lambda to get all of them

;; map is a function , all arguments are evaluated , so thats no problem.
;; no macro-logy needed there.

;; trick is to slurp all arguments on entry
;; then pass to helper function that takes two arguments
;; iterate over the arguments
;; we use one 

;; map f  xs
;; map f2 xs ys
;; map f3 xs ys zs


;; ;; initial mymap is SLURPY
;; (define map
;;   (lambda (f . vals)
;;     (letrec ((mymap1   (lambda (f xs)
;; 			 (cond
;; 			  ((null? xs) '())
;; 			  (else (cons (f (car xs))
;; 				      (mymap1 f (cdr xs)))))))
;; 	     (mymap-help
;; 	      (lambda (f xs vals)
;; 		(cond
;; 		 ((null? xs) '())
;; 		 (else (cons (apply f (mymap1 car vals))
;; 			     (mymap-help f (cdr xs) (mymap1 cdr vals))))))))
;;       (cond
;;        ((pair? vals)
;; 	(mymap-help f (car vals) vals))
;;        (else
;; 	(error "map "))))))


;; (define map (lambda (f xs)
;; 	      (cond
;; 	       ((null? xs) xs)
;; 	       (else (cons (f (car xs))
;; 			   (map f (cdr xs)))))))



;; (define not (lambda (x)
;; 	      (if x
;; 		  #f
;; 		  #t)))


;; (define map
;;   (letrec ((all-cars (lambda (xs)
;;                        (cond
;;                         ((null? xs) xs)
;;                         (else (cons (car (car xs))
;;                                     (all-cars (cdr xs)))))))
;;            (all-cdrs (lambda (xs)
;;                        (cond
;;                         ((null? xs) xs)
;;                         (else (cons (cdr (car xs))
;;                                     (all-cdrs (cdr xs)))))))
;;            (mymap3 (lambda (f xs)
;;                      (cond
;;                       ((null? xs) '())
;;                       ((null? (car xs)) '())
;;                       (else 
;;                        ;;(newline)
;;                        (display "xs3 = ")
;;                        (display xs)
;;                        (newline)
;;                        (cons (apply f (all-cars xs))
;;                              (mymap3 f (all-cdrs xs)))))))

;;            (mymap2 (lambda (f . xs)
;;                      ;;(newline)
;;                      (display "xs = ")
;;                      (display xs)
;;                      (newline)
;;                      (cond
;;                       ((null? xs) '())
;;                       ((null? (car xs)) '())                      
;;                       (else                        
;;                        (mymap3 f xs))))))
;;     (list mymap2 mymap3 all-cdrs all-cars)))


;; (define map
;;   (letrec ((mymap2 (lambda (f . xs) (mymap3 f xs)))
;;             (mymap3 (lambda (f xs)
;;                      (newline)
;;                      (display "xs = ")
;;                      (display xs)
;;                      (newline)
;;                      (cond
;;                       ((null? xs) xs)
;;                       (else (cons (f (car xs))
;;                                   (mymap3 f (cdr xs))))))))
;;     mymap2))



;; contract xs = must be a list of lists
;; f must be a function that takes n of such lists 
(define (map f . xs)
  (letrec ((all-cars (lambda (ys)
		       (cond
			((and (pair? ys) (pair? (car ys)))
			 (cons (car (car ys))
				    (all-cars (cdr ys))))
			(else '()))))
	   (all-cdrs (lambda (ys)
		       (cond
			((and (pair? ys)
			      (pair? (car ys)))
			 (cons (cdr (car ys))
			       (all-cdrs (cdr ys))))
			(else '()))))
	   ;; f doesnt change 
	   (map2 (lambda (ys)
	    	   (cond
	    	    ((null? ys) '())
		    ((null? (car ys)) '())		     
		    (else (cons (apply f (all-cars ys))
				(map2 (all-cdrs ys))))))))
    (map2 xs)))




(display "map defined.")
(newline)





