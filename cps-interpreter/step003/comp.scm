


(define *out* (current-output-port))


(define fixnum-shift 2)

(define (shift-left x n )
  (cond
   ((= n 0) x)
   (else 
    (* 2 (shift-left x (- n 1))))))


(define (comp x)
  (cond
   ((number? x)
    (display "mov dword eax , " *out*)
    (display (shift-left x fixnum-shift) *out*)
    (newline *out*))

   ((and (pair? x) (eq? (car x) 'add1))
    (comp (car (cdr x)))
    (display "add dword eax , " *out*)
    (display (shift-left 1 fixnum-shift) *out*)
    (newline *out*))
   
   ((and (pair? x) (eq? (car x) 'sub1))
    (comp (car (cdr x)))
    (display "sub dword eax , " *out*)
    (display (shift-left 1 fixnum-shift) *out*)
    (newline *out*))
   
   (else #f)))



(define (compile-program input output)
  (call-with-input-file input
    (lambda (in-port)
      (let ((expr (read in-port)))
	(call-with-output-file output
	  (lambda (port)
	    (set! *out* port)
	    
	    (newline *out*)
	    (newline *out*)
	    
	    (display "global scheme_entry" *out*)
	    (newline *out*)
	    
	    (display "scheme_entry: " *out*)
	    
	    ;; here compile the expression
	    (comp expr)
	    
	    (display "ret" *out*)
	    (newline *out*)
	    
	    (newline *out*)
	    (newline *out*)
	    ))))))






















