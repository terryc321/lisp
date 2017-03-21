

;; assuming its well formed,
(define assoc
  (letrec ((assoc 
            (lambda (key vals)
              (cond
               ((null? vals) #f)
               ((eq? key (car (car vals))) (car vals))
               (else (assoc key (cdr vals)))))))
    assoc))







