
(define (1+ x) (+ x 1))

(define (collatz n)
  (display n)
  (newline)
  (cond
   ((equal? n 1) 1)
   ((even? n) (collatz (round (/ n 2))))
   (else (collatz (1+ (* 3 n))))))

