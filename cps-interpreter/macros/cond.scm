

;; install COND macro
;; assuming a well formed cond macro ?
;; (cond (c1 e1a e1b e1c)(ELSE else1 else2 else3))
;; (if c1 (begin e1a e1b e1c) ...)
;;
;; install cond macro
(install-macro
 'cond
 (lambda (expr)
  ;; (cond ...
  (define (c expr)
    (c2 (cdr expr)))
  ;; ((c1 ...)(c2 ...)(else ...))
  (define (c2 expr)
    (let ((condition (car (car expr)))
	  (consequences (cdr (car expr))))
      (cond
       ((eq? condition 'else)
	(cons 'begin consequences))
       (else (list 'if
		   condition
		   (cons 'begin consequences)
		   (c2 (cdr expr)))))))
  (c expr)))



