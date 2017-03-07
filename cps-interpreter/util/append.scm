

;; (append)
;; (append '(1 2 3))
;; (append '(1 2 3) '(4 5 6))
;; (append '(1 2 3) '(4 5 6) '(7 8 9))
;; (append '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12))
(define append
  (lambda args
    (letrec ((app2 (lambda (xs ys)
		     (cond
		      ((null? xs) ys)
		      (else (cons (car xs)
				  (app2 (cdr xs) ys))))))
	     (app (lambda (xs)
		    (cond
		     ;; no lists
		     ((null? xs) '())
		     ;; 1st list only then
		     ((null? (cdr xs)) (car xs))
		     ;; more than 1 list
		     (else (app2 (car xs)
				 (app (cdr xs))))))))
      (app args))))

