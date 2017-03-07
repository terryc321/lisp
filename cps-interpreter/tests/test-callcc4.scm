

(begin 
(define a 1)
(define k1 #f)
(set! k1 (call/cc (lambda (k) k)))
(format #t "hello world a = ~a ~%" a)
(k1 0)
)



