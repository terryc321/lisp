

;; causes infinite loop 
(define a 1)
(define k1 #f)
(define f (lambda ()
	    (call/cc 
               (lambda (k) (set! k1 k)))
	    (format #t "hello world a = ~a ~%" a)))

(f)

(k1 0)



