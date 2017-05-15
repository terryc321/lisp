
;; from rosetta code
;; https://rosettacode.org/wiki/100_doors#Scheme

;; interpreter doesnt like nested definitions yet??

;; There are 100 doors in a row that are all initially closed.
;; You make 100 passes by the doors.
;; The first time through, visit every door and   toggle   the door
;; (if the door is closed,   open it;   if it is open,   close it).
;; The second time, only visit every 2nd door   (door #2, #4, #6, ...),   and toggle it.
;; The third time, visit every 3rd door   (door #3, #6, #9, ...), etc,
;; until you only visit the 100th door.
;; Task : 
;; Answer the question:   what state are the doors in after the last pass?
;; Which are open, which are closed?


;(cons 1 2)
;(cons 1 (cons 2 3))
;(cons 1 (cons 2 (cons 3 4)))

;123
;(+ 1 2)
;(define f 5)
;(+ f f)
;(cons (> f 6) (> f 4))



;;(cons (cons (cons 1 2) (cons 3 4)) (cons (cons 5 6) (cons 7 8)))

;;(1+ 5)
;;(1- 3)

;(cons (+ 1 2) (+ 3 4))
;(car (cons 1 2 ))
;(cdr (cons 1 2 ))

;;(define twice (lambda (n) (+ n n)))
;;(twice 5)
;;(twice (twice 5))


;; (* 2 3 )
;; (- 10 3)
;; (if (< 2 3) 4 5)
;; (if (< 3 2) 4 5)


;; (cons 1 2 )
;; (cons 1 (cons 2 (cons 3 4)))


;; (+ 1 2)
;; (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 (+ 9 10)))))))))



;; (* 1 2)
;; (* 1 (* 2 3))
;; (* 1 (* 2 (* 3 4)))

;; (/ 10 2)

;; (/ (+ 2 10) 3)

;; (not #t)
;; ;(not #f)


;; (car (cons (+ 1 2) (+ 4 5)))

;; (cdr (cons (+ 1 2) (+ 4 5)))

;; (zero? 0)
;; ;(zero? 1)

;; (- 10 7)
;; ;;(- 10 (- 1 2))

;; (boolean? #t)
;; (boolean? 0)

;; (null? ())
;; (null? #f)

;; (if #t (+ 3 4) (+ 7 8))
;; (if #f (+ 9 6) (+ 12 4))
;; (if #f 123)


;; (integer? 123)

;; (let ((a (+ 2 4))) (+ a a))


;; (define f 123)
;; (+ f f)



;; (define g 456)

;; (define h (cons f g))

;; h

;; ;;(even? 5)
;; ;;(even? 6)

;; (cons (cons (odd? 3) (odd? 4))
;;       (cons (even? 3) (even? 4)))


;; (/2 120)

;; (mul3+1 2)

;; (begin)
;; (begin 1 2 3)

;; (= 2 3)
;; ;(= 3 3)

;; (cons (>= 5 6) (cons (>= 6 6) (>= 7 6)))
;; (cons (= 5 6) (cons (= 6 6) (= 7 6)))
;; (cons (<= 5 6) (cons (<= 6 6) (<= 7 6)))
;; (cons (> 5 6) (cons (> 6 6) (> 7 6)))
;; (cons (< 5 6) (cons (< 6 6) (< 7 6)))


;; ;;(fac 10)
;; ((lambda (x) x) 5)


;; (define f2 ((lambda (x) x) 5))




;;()


;3

;; (define (toggle n)  (not n))



;; ;;(begin
;; ;;  (seq 100)
  
;; ;;  (cons (toggle #t) (cons (toggle #f) '())))



;; (define (toggle-every-helper n m xs)
;;   (cond
;;    ((null? xs) '())
;;    ((= n 1) (cons (toggle (car xs))
;; 		  (toggle-every-helper m m (cdr xs))))
;;    (else (cons (car xs)
;; 	       (toggle-every-helper (- n 1) m (cdr xs))))))




;; (define (toggle-every n xs)
;;   (toggle-every-helper n n xs))


;; (define (toggle-nth n xs)
;;   (cond
;;    ((> n 100) xs)
;;    (else (toggle-nth (+ n 1) (toggle-every n xs)))))


;; (define (tog-to-n n xs)
;;   (cond
;;    ((null? xs) '())
;;    ((car xs) (cons n (tog-to-n (+ n 1) (cdr xs))))
;;    (else (tog-to-n (+ n 1) (cdr xs)))))


;; ;;(toggle-nth 1 (seq 100))

;; ;; wrong arity !!
;; ;;(toggle-every 1 1 (seq 100))
;; ;;(toggle-every 2 2 (seq 100))

;; ;;(toggle-every 1 (seq 100))
;; ;;(toggle-every 2 (seq 100))

;; ;; all initially closed - ie #f closed door , #t open door

;; ;;(toggle-nth 1 (seq 1000))

;; (tog-to-n 1 (toggle-nth 1 (seq 100)))




;; (define lim 100)


;; (define init
;;   (letrec ((str (lambda (n)
;; 		  (if (> n lim)
;; 		      '()
;; 		      (cons #f (str (+ 1 n)))))))
;;     (str 1 100)))

;; (define toggle not)
;; ;; map toggle init



;; (define (toggle-every n xs)
;;   (letrec ((tog (lambda (m lim ys)
;; 		  (cond
;; 		   ((null? ys) '())		   
;; 		   ((> m 1) (cons (car ys)
;; 				  (tog (- m 1) lim (cdr ys))))
;; 		   (else (cons (toggle (car ys))
;; 			       (tog lim lim (cdr ys))))))))
;;     (tog n n xs)))


;; (define (toggle-loop n xs)
;;   (display "n = ")
;;   (display n)
;;   (newline)
;;   (display "xs = ")
;;   (display xs)
;;   (newline)
;;   (newline)
;;   (cond
;;    ((> n 100) xs)
;;    (else (toggle-loop (+ n 1) (toggle-every n xs)))))


;; (define solution (toggle-loop 1 init))

;; solution



;; (define (n-doors N)
;;   (define (toggle x str)
;;     (define (s n lis)
;;       (define (revert x)
;;         (if (eq? x 0) 1 0))
;;       (cond ((null? lis) '())
;;           ((zero? (remainder n x)) (cons (revert (car lis)) (s (+ n 1) (cdr lis))))
;;           (else (cons (car lis) (s (+ n 1) (cdr lis))))))
;;     (s 1 str))
;;   (define (iterate x lis)
;;     (if (> x N) lis (iterate (+ x 1) (toggle x lis))))
;;   (iterate 1 (init)))


;; (n-doors 10)


;; (define fib (lambda (n)
;; 	      (if (= n 1)
;; 		  1
;; 		  (if (= n 2)
;; 		      1
;; 		      (+ (fib (- n 1))
;; 			 (fib (- n 2)))))))

;;(fib 10)


;; (define fac (lambda (n)
;; 	      (if (< n 2)
;; 		  1
;; 		  (* n (fac (- n 1))))))

;;(cons (fac 10) (fib 10))


;;(fib 10)
;; (cons 
;;  (cons (fac 10) (cons (fac 9) (fac 5)))
;;  (cons (fib 10) (cons (fib 9) (fib 5))))



;; ;; list of hundred items 
;; (define seq (lambda (n)
;;   (cond
;;    ((= n 0) '())
;;    (else (cons #f (seq (- n 1)))))))

;; (seq 100)


;; lambdas in toplevel LETs behave Oddly also.


;; (define fac2 (lambda (n m)
;; 	      (if (< n 2)
;; 		  m
;; 		  (fac2 (- n 1) (* n m)))))


;; (fac2 10 1)


;; (define fac3 (lambda (n m)
;; 	      (if (< n 2)
;; 		  m
;; 		  (tailcall (fac3 (- n 1) (* n m))))))

;; (cons 
;;  (fac3 5 1)
;;  (cons 
;;   (fac3 6 1)
;;   (cons
;;    (fac3 7 1)
;;    (fac3 8 1))))


;; (define t2 (lambda ()
;; 	     ((let ((g1 123))
;; 		(let ((f3 (lambda () g1)))
;; 		  f3)))))

;; (t2)


;; (+ (fac3 7 1)
;;    (fac3 8 1))

;; (let ((g1 123))
;;   (let ((f3 (lambda () g1)))
;;     (f3)))


;; (let ((g1 123))
;;   (let ((g2 234))
;;     g1))
   
;; (let ((g1 123))
;;   (let ((g2 234))
;;     g2))


;; ((let ((g1 123))
;;   (let ((f3 (lambda () g1)))
;;     f3)))


;; ((lambda (x) (+ x 1)) 10)

(let ((g1 123))
  ((lambda () (+ g1 g1))))



