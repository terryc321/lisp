



;;(letrec ((fib (lambda (n) (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2))))))) (fib 10))
;;(letrec ((odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))) (even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))) (odd? 33))
;; (letrec ((odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))) (even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))) (even? 33))
;; (define map (lambda (f xs) (if (null? xs) xs (cons (f (car xs)) (map f (cdr xs))))))

