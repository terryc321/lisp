


(define *out* (current-output-port))


(define fixnum-shift 2)

(define (shift-left x n )
  (cond
   ((= n 0) x)
   (else 
    (* 2 (shift-left x (- n 1))))))


(define (primitive-character-tag)
  ;; binary 0    0   0    0  1  1  1   1
  ;;       128  64  32   16  8  4  2   1
  ;;
  (+ 1 2 4 8))

(define primitive-character-shift 8)

(define (primitive-boolean-tag)
  ;;       128 | 64  32   16  8  4  2   1
  ;; binary 0  |  0   0    1  1  1  1   1
  (+ 1 2 4 8 16))

(define primitive-boolean-shift  7)
;;       128   | 64  32   16  8  4  2   1
;;             |  0   0    1  1  1  1   1
;;   ----------------------------------------
;;       #t = 159
;;        1    |  0   0    1  1  1  1   1
;;        (+ 1 2 4 8 16 128) 
;;   ----------------------------------------
;;       #f = 31
;;        0    |  0   0    1  1  1  1   1
;;        (+ 1 2 4 8 16) 
;;

;;; Important to remember these are the PRIMITIVE routines of the system 
;;; Therefore are not subject to evaluator , they do their stuff at machine level


(define (comp x)
  (cond

   ((boolean? x)
    (if x 
	(begin ;; true
	  (display "mov dword eax , " *out*)
	  (display (+ (shift-left 1 primitive-boolean-shift)  (primitive-boolean-tag)) *out*)
	  (newline *out*))
	(begin ;; false
	  (display "mov dword eax , " *out*)
	  (display (+ 0 (primitive-boolean-tag)) *out*)
	  (newline *out*))))
   
   ((char? x)
    (display "mov dword eax , " *out*)
    (display (+ (shift-left (char->integer x) primitive-character-shift)
		(primitive-character-tag))
	     *out*)
    (newline *out*))   
    
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

   ((and (pair? x) (eq? (car x) 'integer->char))
    (comp (car (cdr x)))
    
    (display "shl dword eax , 6" *out*)
    (newline *out*)

    (display "add dword eax , " *out*)
    (display (primitive-character-tag) *out*)
    (newline *out*))

   ((and (pair? x) (eq? (car x) 'char->integer))
    (comp (car (cdr x)))
    
    (display "shr dword eax , 6" *out*)
    (newline *out*)
    )
   
   
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






















