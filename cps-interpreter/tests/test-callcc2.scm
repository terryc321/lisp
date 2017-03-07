

;; here is an illustration of way guile enters an infinite loop
;; but i think it should just

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
;(k 0)
;(k 0)
;(k 0)



