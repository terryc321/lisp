

(define (fib n)
  (if (= n 0) 0
      (if (= n 1) 1
	  (if (= n 2) 1
	      (+ (fib (- n 1)) (fib (- n 2)))))))

(fib 10)


