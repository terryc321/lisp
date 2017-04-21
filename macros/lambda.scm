


;; lamb-da expander
(install-macro 'lambda (lambda (expr)
		    (append (list 'lambda (cadr expr))
			    (map macro-expand (cddr expr)))))

