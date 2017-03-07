
;; test call/cc 12

;; but if we save the continuation elsewhere we can get the looping
;; back because set! assignment will only alter one variable binding
;; and not two variable bindings.

;; so we can prove this by setting (k1 k1) to (k1 'bloomin-heck)

(define a 1)
(define k1 #f)
(define k2 #f)
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
    (begin
      ;; save continuation into k2
      (set! k2 k1)
      (k1 'bloomin-heck)
      ;; restore continuation from k2 into k1
      ;; so k1 should now be able to go again
      (set! k1 k2))
    (begin
    (format #t "----- k1 is no longer a procedure: k1 = ~a ~%" k1)))


















