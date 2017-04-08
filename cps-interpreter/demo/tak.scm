

(define (tak x y z)
  (if (< y x)
      (tak (tak (- x 1) y z)
	   (tak (- y 1) z x)
	   (tak (- z 1) x y))
      z))


(do ((z 1 (+ z 1)))
    ((> z 12) #t)  
  (do ((y 1 (+ y 1)))
      ((> y 12) #t)
    (do ((x 1 (+ x 1)))
	((> x 12) #t)
      (let ((result (tak x y z)))
	(if (and (not (= x result))
		 (not (= y result))
		 (not (= z result)))
	    (begin
	      (display "test_case(")
	      (display "\"(tak ") (display x) (display " ") (display y) (display " ") (display z) (display ")\"")
	      (display ",") (display "\"") (display (tak x y z)) (display "\")") (newline))
	    (begin #f))))))

















