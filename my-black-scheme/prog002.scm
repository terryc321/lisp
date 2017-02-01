
(+ 2 1)
(define inc (lambda (x) (+ x 1)))
(inc 2)
((lambda (x) (+ x 1)) 2)
(map inc '(1 2 3))
(map (lambda (x) (+ x 1)) '(1 2 3))


