

;;; translate s expression to nasm


;; just do MAP TRANSLATE code-list
(define (gen x)
  (cond
   ((and (pair? x) (eq? (car x) 'mov) (= (length x) 3))
    (gen-mov x))
   (else
    (error "CODE :GEN: unknown expr " x))))


(define (gen-mov x)
  #t)

(define (gen-add x)
  #t)

(define (gen-sub x)
  #t)

(define (gen-mul x)
  #t)

(define (gen-div x)
  #t)

(define (gen-push x)
  #t)

(define (gen-pop x)
  #t)






     
