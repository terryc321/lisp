
(define calc
     (lambda (x)
       (if (integer? x)
	   x
	   (record-case x
			(+ (x y) (+ (calc x) (calc y)))
			(* (x y) (* (calc x) (calc y)))
			(- (x) (- 0 (calc x)))
			(else (error "invalid expression"))))))



