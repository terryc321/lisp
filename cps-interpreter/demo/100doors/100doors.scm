
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



(define lim 100)


(define init
  (letrec ((str (lambda (n)
		  (if (> n lim)
		      '()
		      (cons #f (str (+ 1 n)))))))
    (str 1 100)))

(define toggle not)
;; map toggle init

(define (toggle-every n xs)
  (letrec ((tog (lambda (m lim ys)
		  (cond
		   ((null? ys) '())
		   ((> m 1) (cons (car ys)
				  (tog (- m 1) lim (cdr ys))))
		   (else (cons (toggle (car ys))
			       (tog lim lim (cdr ys))))))))
    (tog n n xs)))


(define (toggle-loop n xs)
  (display "n = ")
  (display n)
  (newline)
  (display "xs = ")
  (display xs)
  (newline)
  (newline)
  (cond
   ((> n 100) xs)
   (else (toggle-loop (+ n 1) (toggle-every n xs)))))


(define solution (toggle-loop 1 init))

solution



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





