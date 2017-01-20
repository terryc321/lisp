

(define (last xs)
  (cond
   ((null? xs) '())
   ((null? (cdr xs)) (car xs))
   (else (last (cdr xs)))))


