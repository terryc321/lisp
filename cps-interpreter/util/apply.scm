
;; it was a nice idea , problem is that evaluator called base-eval
;; will evaluate the arguments too many times , so this is a primitive .


;; (define app4
;;   (lambda (xs)
;;     ;;(newline)
;;     ;;(display "APP4. xs = ")
;;     ;;(display xs)
;;     ;;(newline)
;;     (cond
;;      ((null? xs) xs)
;;      ((null? (cdr xs)) (car xs))
;;      (else (cons (car xs)
;;                  (app4 (cdr xs)))))))

;; (define app2
;;   (lambda (f . args)
;;     ;;(newline)
;;     ;;(display "APP2. args = ")
;;     ;;(display args)
;;     ;;(newline)
;;     (let ((slurped-args (app4 args)))
;;       ;;(newline)
;;       ;;(display "slurped args = ")
;;       ;;(display slurped-args)
;;       ;;(newline)
;;       ;;(primitive-apply f slurped-args))))
;;       (eval (cons f slurped-args)))))


;; (define apply
;;   (letrec ((app (lambda (f . args)
;;                   (primitive-apply f (slurp args))))
;;            (slurp (lambda (xs)
;;                     (cond
;;                      ((null? xs) xs)
;;                      ((null? (cdr xs)) (car xs))
;;                      (else (cons (car xs)
;;                                  (slurp (cdr xs))))))))
;;     app))


;;(define (apply f . xs)
;;  (f xs))












                    








