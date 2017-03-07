


;; when --> if
(install-macro
 'when
 (lambda (expr)
   `(if ,(car (cdr expr))
	(begin ,@(cddr expr))
	#f)))



