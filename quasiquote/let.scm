

(define (klet-rewrite exp)
  (let ((args (cadr exp))
	(body (cddr exp)))
    `((lambda ,(map car args) ,@body) ,@(map (lambda (x) `(begin ,@(cdr x))) args))))





