

;; let*
;; (let* ((a 1 2 3)(b 4 5 6)(c 7 8 9)) ... )
;; ->
;; (let ((a 1 2 3)) (let ((b 4 5 6)) (let ((c 7 8 9)) ...)))
;; ->
;;
(install-macro
 'let*
 (lambda (expr)
   (define (c expr)
     (c2 (car (cdr expr))  (cddr expr)))

   (define (c2 varvals body)
     (cond
      ((null? varvals) (cons 'begin body))
      (else `(let (,(car varvals)) ,(c2 (cdr varvals) body)))))
   (c expr)))

