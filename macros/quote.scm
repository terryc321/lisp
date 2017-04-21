


;; quote , which when evaluated is itself
(install-macro
 'quote
 (lambda (exp)
   `((lambda () ,(car (cdr exp))))))











