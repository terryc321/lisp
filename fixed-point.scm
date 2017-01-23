
;; fixed-point.scm


;; note f is free variable
(define (fixed-point x)
  (sub-fixed-point x (f x)))

(define (sub-fixed-point x y)
  (display x)
  (newline)
  (if (equal? x y)
      x
      (sub-fixed-point y (f y))))


(define (f x)
  (+ (* 1.5 x) (* -0.25 x x x)))


(fixed-point 1.0)

(define r2 (fixed-point 3.0))







