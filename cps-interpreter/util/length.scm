

;; works on lists , and anything else
;; gets caught out by circular lists
;; tail recursive -good.
(define length
  (lambda (xs)
    (letrec ((length-acc
	      (lambda (ys acc)
		(cond
		 ((null? ys) acc)
		 ((pair? ys) (length-acc (cdr ys) (+ 1 acc)))
		 (else (+ 1 acc))))))
      (length-acc xs 0))))


(display "length defined")
(newline)





