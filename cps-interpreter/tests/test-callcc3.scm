

;; here is an illustration of way guile enters an infinite loop
;; wrap all forms in a big begin , this should be sufficient to give the
;; endless looping we require as in guile

(begin

(define a 1)
(define k #f)
(define f (lambda ()
	    (call/cc (lambda (k2)
		      (set! k k2)))
	    (display "hello world : ")
	    (display a)
	    (newline) 
	    (set! a (+ a 1))))

(f)

(k 0)

)


