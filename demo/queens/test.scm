
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


;; ;(cons 1 2)
;; ;(cons 1 (cons 2 3))
;; ;(cons 1 (cons 2 (cons 3 4)))

;; ;123
;; ;(+ 1 2)
;; ;(define f 5)
;; ;(+ f f)
;; ;(cons (> f 6) (> f 4))



;; ;;(cons (cons (cons 1 2) (cons 3 4)) (cons (cons 5 6) (cons 7 8)))

;; ;;(1+ 5)
;; ;;(1- 3)

;; ;(cons (+ 1 2) (+ 3 4))
;; ;(car (cons 1 2 ))
;; ;(cdr (cons 1 2 ))

;; ;;(define twice (lambda (n) (+ n n)))
;; ;;(twice 5)
;; ;;(twice (twice 5))


;; ;; (* 2 3 )
;; ;; (- 10 3)
;; ;; (if (< 2 3) 4 5)
;; ;; (if (< 3 2) 4 5)


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

;; (define fac (lambda (n)
;; 	      (if (< n 2) 1
;; 		  (* n (fac (- n 1))))))

;; ;; THIS OVERFLOWS , luckily it doesnt cause any major problems
;; (fac 25)

;; ;;(fac 5) ;;(fac 5))

;; ;;(fac 4) ;;(fac 5))

;; ()

;; (define (add1 n) (1+ n))
;; (define (sub1 n) (1- n))


;; (define (length xs)
;;   (cond
;;    ((null? xs) 0)
;;    (else (+ 1 (length (cdr xs))))))

;; (length (cons 0 (cons 0 (cons 0 ()))))


;; (and #f #f #f)

;; (and)
;; (and #t)
;; (and #f #f)
;; (and #f #f #f)
;; (and #t #t #t #f)
;; (and #t #t #t #t)


;; ;; (define good
;; ;;   (lambda (new-pl up down)
;; ;;     (cond
;; ;;      ((null? new-pl) #t)
;; ;;      (else (let ((next-pos (car new-pl)))
;; ;; 	     (and
;; ;; 	      (not (= next-pos try))
;; ;; 	      (not (= next-pos up))
;; ;; 	      (not (= next-pos down))
;; ;; 	      (good (cdr new-pl)
;; ;; 		    (add1 up)
;; ;; 		    (sub1 down))))))))



;; ;; (define (good newpl up down)
;; ;;   (cond
;; ;;    ((null? newpl) #t)
;; ;;    (else (let ((nextpos (car newpl)))
;; ;; 	   (and
;; ;; 	    (not (= nextpos try))
;; ;; 	    (not (= nextpos up))
;; ;; 	    (not (= nextpos down))
;; ;; 	    (good (cdr newpl)
;; ;; 		  (add1 up)
;; ;; 		  (sub1 down)))))))


;; (define f (closure (x) () (if (= x 1) 1 (if (= x 2) 1 (+ (f (- x 1)) (f (- x 2)))))))

;; ;;(f 10)

;; ;;(f 20)

;; ;;(f 30)
;; ;;(cons (f 38) (cons (f 39) (f 40)))

;; ;;(f 40)

;; ;;(+ 39088169 63245986)

;; (cond ((= 1 2) 13)
;;       ((= 2 2) 14)
;;       ((= 3 2) 15))



;; (define a-b (+ 123 456))
;; a-b

;; (define a-b2? (+ 3 4))
;; a-b2?

;; (let ((a+b 3))
;;   a+b)

;; (let ((a+b?2 32))
;;   a+b?2)


(define (add1 n) (1+ n))
(define (sub1 n) (1- n))

(define (length xs)
  (cond
   ((null? xs) 0)
   (else (+ 1 (length (cdr xs))))))


(define good?
  (closure (new-pl up down try)  ()
  ;;(lambda (new-pl up down try)
    (cond
     ((null? new-pl) #t)
     (else (let ((next-pos (car new-pl)))
	     (and
	      (not (= next-pos try))
	      (not (= next-pos up))
	      (not (= next-pos down))
	      (good? (cdr new-pl)
		     (add1 up)
		     (sub1 down)
		     try)))))))

(define legal?
  (closure (try legal-pl) ()
    (good? legal-pl (add1 try) (sub1 try) try)))


(define solution?
    (lambda (legal-pl boardsize)
      (= (length legal-pl) boardsize)))


(define build-solution
  (lambda (legal-pl boardsize)
    (cond
      ((solution? legal-pl boardsize) legal-pl)
      (else (forward boardsize legal-pl boardsize)))))


(define forward
  (lambda (try legal-pl boardsize)
    (cond
      ((zero? try) (backtrack legal-pl boardsize))
      ((legal? try legal-pl) (build-solution (cons try legal-pl) boardsize))
      (else (forward (sub1 try) legal-pl boardsize)))))


(define backtrack
  (lambda (legal-pl boardsize)
    (cond
      ((null? legal-pl) '())
      (else (forward (sub1 (car legal-pl))
		     (cdr legal-pl)
		     boardsize)))))


(define build-loop
  (lambda (boardsize sol)
    (cond
     ((null? sol) '())
     (else (cons sol (build-loop boardsize (backtrack sol boardsize)))))))


(define build-all-solutions
  (lambda (boardsize)
    (build-loop boardsize (build-solution '() boardsize))))


;;'()

(define queens
  (lambda (n)
    (build-all-solutions n)))


;;(queens 4)

;;(queens 5)
;;(queens 6)

;;(length (queens 8))


;; queens 12 expected 14200 solutions
(let ((solution (queens 12)))
  (cons solution
	(length solution)))



;; ((4 . (9 . (7 . (2 . (11 . (6 . (12 . (10 . (8 . (5 . (3 . (1 . ()




































