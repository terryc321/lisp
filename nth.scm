

(define (nth k xs)
  (nth-helper 1 k xs))

(define (nth-helper n k xs)
  (cond
   ((>= n k) (car xs))
   (else (nth-helper (+ n 1) k (cdr xs)))))


;; 
(nth 26 '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

;; equational reasoning
(define (last xs)
  (nth (length xs) xs))


(last '(a b c d e f g h i j k l m n o p q r s t u v w x y z))





