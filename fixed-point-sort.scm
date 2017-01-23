
;; fixed-point.scm

(define (atom? x) (not (pair? x)))


;; note f is free variable
(define (fixed-point x)
  (sub-fixed-point x (f x)))

(define (sub-fixed-point x y)
  (display x)
  (newline)
  (if (equal? x y)
      x
      (sub-fixed-point y (f y))))



(define (sort numbers)
  (fixed-point numbers))

(define (f nums)
  (cond
   ((or (atom? nums) (atom? (cdr nums)))
    nums)
   ((< (car nums) (cadr nums))
    (cons (car nums) (f (cdr nums))))
   (else
    (cons (cadr nums)
	  (f (cons (car nums)
		   (cddr nums)))))))

(f '(1 9 2 8 3 7 4 6 5 ))

(sort '(1 9 2 8 3 7 4 6 5 ))



