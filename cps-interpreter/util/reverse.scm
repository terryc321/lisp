

;; (reverse)
(define reverse
  (lambda (xs)
    (letrec ((rev-acc (lambda (ys acc)
			(cond
			 ((null? ys) acc)
			 ((pair? ys) (rev-acc (cdr ys) (cons (car ys) acc)))
			 (else acc)))))
      (rev-acc xs '()))))

(display "reverse defined")
(newline)





