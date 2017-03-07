
;; test call/cc 6

(define a 1)
(define k1 #f)
(define f (lambda ()
	    (call/cc 
               (lambda (k) (set! k1 k)))
	    (format #t "hello world a = ~a ~%" a)
            (set! a (+ a 1))))


;; cant just call k1 now as its bound to false
;;(k1 0)

(f)
;; now we can call k1 as it has the continuation

(k1 0)





