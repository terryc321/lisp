
;; ---------------------- map --------------------------------------------
;; surprisingly difficult to get this right , really.
;; map is a function , all arguments are evaluated .
;; map f  xs
;; map f2 xs ys
;; map f3 xs ys zs


(define mymap1
  (lambda (f xs)
    (cond
     ((null? xs) '())
     (else (cons (f (car xs))
		 (mymap1 f (cdr xs)))))))

;; initial mymap is SLURPY
(define mymap
  (lambda (f . vals)
    (display "vals = ")
    (display vals)
    (newline)
    (mymap-help f (car vals) vals)))

;; help is not SLURPY
(define mymap-help
  (lambda (f xs vals)
    (display "vals = ")
    (display vals)
    (newline)
    (cond
     ((null? xs) '())
     (else (cons (apply f (mymap1 car vals))
		 (mymap-help f (cdr xs) (mymap1 cdr vals)))))))

(define check (lambda args args))


(define twice (lambda (x) (+ x x)))
(display "test 1 : ")
(display (mymap twice '(1 2 3 4 5)))
(newline)


(define add2 (lambda (x y) (+ x y)))
(display "test 2 : ")
(display (mymap add2 '(1 2 3 4 5) '(10 20 30 40 50)))
(newline)

(define add3 (lambda (x y z)  (+ x (+ y z))))
(display " test 3 : ")
(display (mymap add3 '(1 2 3 4 5) '(10 20 30 40 50) '(100 200 300 400 500)))
(newline)

(define add4 (lambda (x y z a)  (+ x (+ y (+ z a)))))
(display " test 4 : ")
(display (mymap add4
		'(1 2 3 4 5)
		'(10 20 30 40 50)
		'(100 200 300 400 500)
		'(1000 2000 3000 4000 5000)))
(newline)


(define add5 (lambda (x y z a b)  (+ x (+ y (+ z (+ a b))))))
(display " test 5 : ")
(display (mymap +
		'(1 2 3 4 5)
		'(10 20 30 40 50)
		'(100 200 300 400 500)
		'(1000 2000 3000 4000 5000)
		'(10000 20000 30000 40000 50000)))
(newline)









