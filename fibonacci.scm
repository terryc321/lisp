

(define (fibonacci n) 
  (cond
   ((< n 3) 1)
   (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

;; looping sum version also
;; direct using square roots



