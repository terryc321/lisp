
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


;; list of hundred items 
(define (seq n)
  (cond
   ((= n 0) '())
   (else (cons #f (seq (- n 1))))))


(define (toggle n)
  (not n))


;;(begin
;;  (seq 100)
  
;;  (cons (toggle #t) (cons (toggle #f) '())))



(define (toggle-every-helper n m xs)
  (cond
   ((null? xs) '())
   ((= n 1) (cons (toggle (car xs))
		  (toggle-every-helper m m (cdr xs))))
   (else (cons (car xs)
	       (toggle-every-helper (- n 1) m (cdr xs))))))


(define (toggle-every n xs)
  (toggle-every-helper n n xs))


(define (toggle-nth n xs)
  (cond
   ((> n 100) xs)
   (else (toggle-nth (+ n 1) (toggle-every n xs)))))


(define (tog-to-n n xs)
  (cond
   ((null? xs) '())
   ((car xs) (cons n (tog-to-n (+ n 1) (cdr xs))))
   (else (tog-to-n (+ n 1) (cdr xs)))))

;;(toggle-every 1 1 (seq 100))
;;(toggle-every 2 2 (seq 100))

;; all initially closed - ie #f closed door , #t open door

;;(toggle-nth 1 (seq 1000))

(tog-to-n 1 (toggle-nth 1 (seq 100)))


























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




