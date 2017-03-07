
;; test call/cc 9

;; taking the value of a call/cc expression gives a one-shot continuation

;; wrapping forms in a begin has no difference
(begin

(define a 1)
(define k1 #f)
(define k2 #f)
(define f (lambda ()
;; here try get value from call/cc
	    (set! k1 (call/cc 
		      (lambda (k) k)))
            (set! k2 (call/cc
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
    (k1 k2)
    (format #t "----- k1 is no longer a procedure: k1 = ~a ~%" k1))
)


;;k2 
;; now we can call k2 if it has the continuation
(if (procedure? k2)
    (k2 k1)
    (format #t "----- k2 is no longer a procedure: k2 = ~a ~%" k2))
)












