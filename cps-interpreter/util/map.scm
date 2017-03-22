
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


(define map
  (letrec ((all-cars (lambda (xs)
                       (cond
                        ((null? xs) xs)
                        (else (cons (car (car xs))
                                    (all-cars (cdr xs)))))))
           (all-cdrs (lambda (xs)
                       (cond
                        ((null? xs) xs)
                        (else (cons (cdr (car xs))
                                    (all-cdrs (cdr xs)))))))
           (mymap3 (lambda (f xs)
                     (cond
                      ((null? xs) '())
                      ((null? (car xs)) '())
                      (else 
                       ;;(newline)
                       (display "xs3 = ")
                       (display xs)
                       (newline)
                       (cons (apply f (all-cars xs))
                             (mymap3 f (all-cdrs xs)))))))

           (mymap2 (lambda (f . xs)
                     ;;(newline)
                     (display "xs = ")
                     (display xs)
                     (newline)
                     (cond
                      ((null? xs) '())
                      ((null? (car xs)) '())                      
                      (else                        
                       (mymap3 f xs))))))
    mymap2))


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
































