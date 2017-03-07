
;; test call/cc 10

;; 

(define a 1)
(define k1 #f)
(define f (lambda ()
;; here try get value from call/cc
;; continuation of call/cc is to assign that to k1
;; so if we pass in 42 say , it works once , but then k1 is 42 and 
;; it cant be called again , like you have lost the continuation.
	    (set! k1 (call/cc 
		      (lambda (k) k)))
            (format #t "k1 has binding ~A ~%" k1)
	    (format #t "hello world a = ~a ~%" a)
            (set! a (+ a 1))))


;; cant just call k1 now as its bound to false
;;(k1 0)

;; setup k1 by calling f
(f)

(format #t " ------ now call k1 continuation ----- ~% ")

;; k1 
;; now we can call k1 as it has the continuation
(if (procedure? k1)
    (k1 k1)
    (format #t "----- k1 is no longer a procedure: k1 = ~a ~%" k1))














