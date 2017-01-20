

;; append 
(define (append xs ys)
  (cond
   ((null? xs) ys)
   (else (cons (car xs) (append (cdr xs) ys)))))




