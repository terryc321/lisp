

;; append 
(define (append xs ys)
  (cond
   ((null? xs) ys)
   (else (cons (car xs) (append (cdr xs) ys)))))


;; 
(define (reverse xs)
  (cond
   ((null? xs) xs)
   (else (append (reverse (cdr xs)) (cons (car xs) '())))))




