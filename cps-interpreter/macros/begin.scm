

;; begin macro
;; (begin x) = x
;; (begin x y z) = (begin x y z)

(install-macro
 'begin
 (lambda (exp)
   (cond
    ((= (length exp) 1) #f)
    ((= (length exp) 2) (car (cdr exp)))
    (else `(begin ,@(map macro-expand (cdr exp)))))))
























