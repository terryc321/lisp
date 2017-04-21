

;; letrec
(install-macro
 'letrec
 (lambda (expr)
   (define (c expr)
     (c2 (car (cdr expr))  (cddr expr)))
   (define (c2 varvals body)
     (cond
      ((null? varvals) (cons 'begin body))
      (else (append
	     (list 'let)
	     (list (map (lambda (x) (list (car x) #f)) varvals))
	     (list (append (list 'begin)
			   (append
			    (map (lambda (x) (list 'set! (car x) (cons 'begin (cdr x)))) varvals)
			    body)))))))
   (c expr)))
