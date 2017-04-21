



(define *out* (current-output-port))

(define the-empty-list-value 47)
(define the-false-value 31)


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

(define the-boolean-tag
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

(define the-boolean-mask
  (+ 64 32 16 8 4 2 1))


(define the-integer-mask
  (+ 1 2))

(define the-integer-tag 0)


;;(+ 1 2 4 8 16) #f
;;(+ 1 2 4 8 16 128) #t

;;; Important to remember these are the PRIMITIVE routines of the system 
;;; Therefore are not subject to evaluator , they do their stuff at machine level



(define (comp x)
  (cond

   ((null? x)
    (display "mov dword eax , " *out*)
    (display the-empty-list-value *out*)
    (newline *out*))

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
    (newline *out*))

   ((and (pair? x) (eq? (car x) 'zero?))
    (comp (car (cdr x)))
    
    (display "cmp dword eax , 0" *out*)
    (newline *out*)
    (display "mov dword eax , 0" *out*)
    (newline *out*)
    (display "sete al" *out*)
    (newline *out*)
    (display "shl dword eax , " *out*)
    (display primitive-boolean-shift  *out*)
    (newline *out*)
    (display "or dword eax , " *out*)
    (display (primitive-boolean-tag) *out*)
    (newline *out*))

   
   ((and (pair? x) (eq? (car x) 'null?))
    (comp (car (cdr x)))
    
    (display "cmp dword eax , " *out*)
    (display the-empty-list-value *out*)
    (newline *out*)
    (display "mov dword eax , 0" *out*)
    (newline *out*)
    (display "sete al" *out*)
    (newline *out*)
    (display "shl dword eax , " *out*)
    (display primitive-boolean-shift  *out*)
    (newline *out*)
    (display "or dword eax , " *out*)
    (display (primitive-boolean-tag) *out*)
    (newline *out*))

   ((and (pair? x) (eq? (car x) 'not))
    (comp (car (cdr x)))
    
    (display "cmp dword eax , " *out*)
    (display the-false-value *out*)
    (newline *out*)
    
    (display "mov dword eax , 0" *out*)
    (newline *out*)
    
    (display "sete al" *out*)
    (newline *out*)
    
    (display "shl dword eax , " *out*)
    (display primitive-boolean-shift  *out*)
    (newline *out*)
    
    (display "or dword eax , " *out*)
    (display (primitive-boolean-tag) *out*)
    (newline *out*))
   

   ((and (pair? x) (eq? (car x) 'boolean?))
    (comp (car (cdr x)))
    
    (display "and dword eax , " *out*)
    (display the-boolean-mask *out*)
    (newline *out*)

    (display "cmp dword eax , " *out*)
    (display the-boolean-tag *out*)
    (newline *out*)
    
    (display "mov dword eax , 0" *out*)
    (newline *out*)
    
    (display "sete al" *out*)
    (newline *out*)
    
    (display "shl dword eax , " *out*)
    (display primitive-boolean-shift  *out*)
    (newline *out*)
    
    (display "or dword eax , " *out*)
    (display (primitive-boolean-tag) *out*)
    (newline *out*))   


   ((and (pair? x) (eq? (car x) 'integer?))
    (comp (car (cdr x)))
    
    (display "and dword eax , " *out*)
    (display the-integer-mask *out*)
    (newline *out*)

    (display "cmp dword eax , " *out*)
    (display the-integer-tag *out*)
    (newline *out*)
    
    (display "mov dword eax , 0" *out*)
    (newline *out*)
    
    (display "sete al" *out*)
    (newline *out*)
    
    (display "shl dword eax , " *out*)
    (display primitive-boolean-shift  *out*)
    (newline *out*)
    
    (display "or dword eax , " *out*)
    (display (primitive-boolean-tag) *out*)
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





















