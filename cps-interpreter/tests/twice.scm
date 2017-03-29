

(define twice (lambda (x) (+ x x)))

(twice 5)
(twice (twice 4))

(define (twice2 x) (+ x x))

(twice2 5)
(twice2 (twice2 4))



