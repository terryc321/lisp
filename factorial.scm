

(define (factorial n)
  (cond
   ((< n 2) 1)
   (else (* n (factorial (- n 1))))))


