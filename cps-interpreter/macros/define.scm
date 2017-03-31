

;; define macro

;; (define (f x) ...) => (define f (lambda (x) ...))

(install-macro
 'define
 (lambda (exp)
   (cond
    ((symbol? (car (cdr exp)))
     (let ((body (map macro-expand (cdr (cdr exp)))))
       `(define ,(car (cdr exp)) ,@body)))
    (else (let ((operator (car (car (cdr exp))))
		(operands (cdr (car (cdr exp))))
		(body (map macro-expand (cdr (cdr exp)))))
	    `(define ,operator (lambda ,operands ,@body)))))))
























