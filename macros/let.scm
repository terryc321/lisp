


;; let macro
;; (let ((a 1 2 3)(b 2 3 4)(c 5 6 7)) ... )
;; ((lambda (a b c) ...) (begin 1 2 3)(begin 2 3 4)(begin 5 6 7))
(install-macro
 'let
 (lambda (expr)
   (append (list (append (list 'lambda)
			 (list (map car (cadr expr)))
			 (cddr expr)))  
	   (map (lambda (x) (cons 'begin (cdr x))) (cadr expr)))))

