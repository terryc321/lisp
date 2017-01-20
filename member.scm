

;; member.scm

(define (member x ys)
  (cond
   ((null? ys) #f)
   ((equal? x (car ys)) ys)
   (else (member x (cdr ys)))))


(member 'a '(a b c d e f))

(member 'd '(a b c d e f))


