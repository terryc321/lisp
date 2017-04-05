



;; on 32bit system *wordsize* is 4 , as 4 bytes 4 x 8bits per byte = 32 bits in total
;; on 64bit system *wordsize* is 8 , as 8 bytes 8 x 8bits per byte = 64 bits in total
(define *wordsize*  4)


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


(define (comp-null x si)
  (display "mov dword eax , " *out*)
  (display the-empty-list-value *out*)
  (newline *out*))


(define (comp-boolean x si)
  (if x 
      (begin ;; true
	(display "mov dword eax , " *out*)
	(display (+ (shift-left 1 primitive-boolean-shift)  (primitive-boolean-tag)) *out*)
	(newline *out*))
      (begin ;; false
	(display "mov dword eax , " *out*)
	(display (+ 0 (primitive-boolean-tag)) *out*)
	(newline *out*))))


(define (comp-char x si)
  (display "mov dword eax , " *out*)
  (display (+ (shift-left (char->integer x) primitive-character-shift)
	      (primitive-character-tag))
	   *out*)
  (newline *out*))


(define (comp-integer x si)
  (display "mov dword eax , " *out*)
  (display (shift-left x fixnum-shift) *out*)
  (newline *out*))

(define (comp-add1 x si)
  (comp (car (cdr x)) si)
  (display "add dword eax , " *out*)
  (display (shift-left 1 fixnum-shift) *out*)
  (newline *out*))

(define (comp-sub1 x si)
  (comp (car (cdr x)) si)
  (display "sub dword eax , " *out*)
  (display (shift-left 1 fixnum-shift) *out*)
  (newline *out*))

(define (comp-integer->char x si)
  (comp (car (cdr x)) si)
  (display "shl dword eax , 6" *out*)
  (newline *out*)
  (display "add dword eax , " *out*)
  (display (primitive-character-tag) *out*)
  (newline *out*))

(define (comp-char->integer x si)
  (comp (car (cdr x)) si)
  (display "shr dword eax , 6" *out*)
  (newline *out*))


(define (comp-zero? x si)
  (comp (car (cdr x)) si)
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


(define (comp-null? x si)
  (comp (car (cdr x)) si)
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


(define (comp-not x si)
  (comp (car (cdr x)) si)
  
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




(define (comp-boolean? x si)
  (comp (car (cdr x)) si)
  
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


(define (comp-integer? x si)
  
    (comp (car (cdr x)) si)
    
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


(define (comp-add x si)
    ;; (+ x y)
    ;; E[x]
    (comp (car (cdr x)) si)
    ;; save onto stack
    (if (< si 0)
	(begin
	  (display "mov dword [ esp " *out*)
	  (display si *out*)
	  (display "] , eax " *out*)
	  (newline *out*))
	(begin
	  (error "si stack index 0 or more - comp-add ?? ")))
    ;; E[y]
    (comp (car (cdr (cdr x))) (- si *wordsize*))
    (if (< si 0)
	(begin
	  (display "add dword eax , [ esp " *out*)
	  (display si *out*)
	  (display "] " *out*)
	  (newline *out*))
	(begin
	  (error "si stack index 0 or more - comp-add ?? "))))



(define (comp-sub x si)
    ;; (- x y)
    ;; E[x]
    (comp (car (cdr x)) si)
    ;; save onto stack
    (if (< si 0)
	(begin
	  (display "mov dword [ esp " *out*)
	  (display si *out*)
	  (display "] , eax " *out*)
	  (newline *out*))
	(begin
	  (error "si stack index 0 or more - comp-sub ?? ")))
    ;; E[y]
    (comp (car (cdr (cdr x))) (- si *wordsize*))
    (if (< si 0)
	(begin
	  (display "sub dword [ esp " *out*)
	  (display si *out*)
	  (display "] , eax " *out*)
	  (newline *out*)

	  (display "mov dword eax , [ esp " *out*)
	  (display si *out*)
	  (display "] " *out*)
	  (newline *out*))
	(begin
	  (error "si stack index 0 or more - comp-add ?? "))))



(define (comp x si)
  (cond
   ((null? x) (comp-null x si))
   ((boolean? x)  (comp-boolean x si))
   ((char? x) (comp-char x si))    
   ((integer? x) (comp-integer x si))
   ((and (pair? x) (eq? (car x) 'add1)) (comp-add1 x si))  
   ((and (pair? x) (eq? (car x) 'sub1)) (comp-sub1 x si))   
   ((and (pair? x) (eq? (car x) 'integer->char)) (comp-integer->char x si))
   ((and (pair? x) (eq? (car x) 'char->integer)) (comp-char->integer x si))
   ((and (pair? x) (eq? (car x) 'zero?)) (comp-zero? x si))
   ((and (pair? x) (eq? (car x) 'null?)) (comp-null? x si))
   ((and (pair? x) (eq? (car x) 'not)) (comp-not x si))
   ((and (pair? x) (eq? (car x) 'boolean?)) (comp-boolean? x si))
   ((and (pair? x) (eq? (car x) 'integer?)) (comp-integer? x si))   
   ((and (pair? x) (eq? (car x) '+)) (comp-add x si))
   ((and (pair? x) (eq? (car x) '-)) (comp-sub x si))
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
	    ;; initial stack index is negative wordsize
	    ;; as [ esp - 4 ] , since esp holds return address.
	    (comp expr (- *wordsize*))
	    
	    (display "ret" *out*)
	    (newline *out*)
	    
	    (newline *out*)
	    (newline *out*)
	    ))))))






















