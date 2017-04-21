










(define fixnum-shift 2)

(define (shift-left x n )
  (cond
   ((= n 0) x)
   (else 
    (* 2 (shift-left x (- n 1))))))


(define (comp x)
  (cond
   ((number? x)
    (display "mov dword eax , ")
    (display (shift-left x fixnum-shift))
    (newline))
   (else #f)))









