

;; define some function f and its derivative df
(define (f x)
  (+ (* 17 x x x x)
     (* -5 x x x)
     (* 3 x)
     -10))

;; the derivative of f above
(define (df x)
  (+ (* 68 x x x)
     (* -15 x x)
     3))

(define (newton+ x)
  (- x (/ (f x) (df x))))


(define (newton-search n lim x close-enough?)
  (display "guess x = ") (display x)
  (display ": f x = ") (display (f x)) (newline)
  (if (close-enough? (f x))
      x
      (if (> n lim)
	  "failed to converge"
	  (newton-search (+ n 1) lim (newton+ x) close-enough?))))


(define (close-enough? x)
  (< (abs x) 1.0e-4 ))

;; simpler newton-search 
(define (newton-search x)
  (if (close-enough? (f x))
      x
      (newton-search (newton+ x))))

(define root (newton-search 100.0))

(f root)








