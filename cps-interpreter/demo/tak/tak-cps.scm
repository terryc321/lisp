


;; cps hand translation of this TAK program

;; (define tak (lambda (x y z)
;; 	      (if (< y x)
;; 		  (tak (tak (- x 1) y z)
;; 		       (tak (- y 1) z x)
;; 		       (tak (- z 1) x y))
;; 		  z)))
;; (tak 18 12 6)

;; Number TAK calls so easier 
;; (define tak (lambda (x y z)
;; 	      (if (< y x)
;; 		  (tak4 (tak1 (- x 1) y z)
;; 		        (tak2 (- y 1) z x)
;; 		        (tak3 (- z 1) x y))
;; 		  z)))
;; (tak 18 12 6)



;; k1 , k2 continuations that dont take any arguments , just jump locations.
(define (k< a b k1 k2)  (if (< a b)  (k1)   (k2)))

(define (k- a b k)  (k (- a b)))

(define (ktak x y z k)
  (k< y x
      (lambda () ; (< y x)
	(k- x 1
	    (lambda (x1) 
	      (ktak x1 y z
		   (lambda (tak1v)
	(k- y 1
	    (lambda (y1) 
	      (ktak y1 z x
		   (lambda (tak2v)
	(k- z 1
	    (lambda (z1) 
	      (ktak z1 x y
		    (lambda (tak3v)
		      (ktak tak1v tak2v tak3v k))))))))))))))
      (lambda () (k z))))


;; identity continuation
(define kid (lambda (x) x))

(ktak 18 12 6 kid)



  

