
;; maximum of numbers

(define (atom? x) (not (pair? x)))

(define (max-1 numbers)
  (cond
   ((null? numbers) '())
   ((atom? (cdr numbers)) (car numbers))
   ((< (max-1 (cdr numbers)) (car numbers))
    (car numbers))
   (else (max-1 (cdr numbers)))))

(max-1 '(3 1 41 5 9))

(define (max-2 numbers)
  (cond
   ((null? numbers) '())
   (else (co-max-2 (car numbers) (max-2 (cdr numbers))))))

(define (co-max-2 m n)
  (cond
   ((null? n) m)
   ((< m n) n)
   (else m)))


