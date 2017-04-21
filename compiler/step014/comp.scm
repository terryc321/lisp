



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


(define (comp-null x si env)
  (display "mov dword eax , " *out*)
  (display the-empty-list-value *out*)
  (newline *out*))


(define (comp-boolean x si env)
  (if x 
      (begin ;; true
	(display "mov dword eax , " *out*)
	(display (+ (shift-left 1 primitive-boolean-shift)  (primitive-boolean-tag)) *out*)
	(newline *out*))
      (begin ;; false
	(display "mov dword eax , " *out*)
	(display (+ 0 (primitive-boolean-tag)) *out*)
	(newline *out*))))


(define (comp-char x si env)
  (display "mov dword eax , " *out*)
  (display (+ (shift-left (char->integer x) primitive-character-shift)
	      (primitive-character-tag))
	   *out*)
  (newline *out*))


(define (comp-integer x si env)
  (display "mov dword eax , " *out*)
  (display (shift-left x fixnum-shift) *out*)
  (newline *out*))

(define (comp-add1 x si env)
  (comp (car (cdr x)) si env)
  (display "add dword eax , " *out*)
  (display (shift-left 1 fixnum-shift) *out*)
  (newline *out*))

(define (comp-sub1 x si env)
  (comp (car (cdr x)) si env)
  (display "sub dword eax , " *out*)
  (display (shift-left 1 fixnum-shift) *out*)
  (newline *out*))

(define (comp-integer->char x si env)
  (comp (car (cdr x)) si env)
  (display "shl dword eax , 6" *out*)
  (newline *out*)
  (display "add dword eax , " *out*)
  (display (primitive-character-tag) *out*)
  (newline *out*))

(define (comp-char->integer x si env)
  (comp (car (cdr x)) si env)
  (display "shr dword eax , 6" *out*)
  (newline *out*))


(define (comp-zero? x si env)
  (comp (car (cdr x)) si env)
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


(define (comp-null? x si env)
  (comp (car (cdr x)) si env)
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


(define (comp-not x si env)
  (comp (car (cdr x)) si env)
  
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




(define (comp-boolean? x si env)
  (comp (car (cdr x)) si env)
  
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


(define (comp-integer? x si env)
  
    (comp (car (cdr x)) si env)
    
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


(define (comp-add x si env)
    ;; (+ x y)
    ;; E[x]
    (comp (car (cdr x)) si env)
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
    (comp (car (cdr (cdr x))) (- si *wordsize*) env)
    (if (< si 0)
	(begin
	  (display "add dword eax , [ esp " *out*)
	  (display si *out*)
	  (display "] " *out*)
	  (newline *out*))
	(begin
	  (error "si stack index 0 or more - comp-add ?? "))))



(define (comp-sub x si env)
    ;; (- x y)
    ;; E[x]
    (comp (car (cdr x)) si env)
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
    (comp (car (cdr (cdr x))) (- si *wordsize*) env)
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


;; (let ...)
(define (comp-let x si env)
  (let ((bindings (car (cdr x)))
	(body (cdr (cdr x)))
	(local-env '()))
    (comp-let-bindings bindings body si env local-env)))


(define (comp-let-bindings bindings body si env local-env)
  (cond
   ;; extend environment with local bindings
   ((null? bindings)
    ;;(display "LET: new local env : ") (display local-env) (newline)    
    (comp-implicit-sequence body si (append local-env env)))
   
   ;; some more bindings to do
   (else (let ((the-binding (car bindings)))
	   (let ((the-sym (car the-binding))
		 (the-sym-body (cdr the-binding)))

	     (comp-implicit-sequence the-sym-body si env)
	     ;; mov eax into si
	     ;; si represents the free slot on stack
	     (display "mov dword [ esp " *out*)
	     (display si *out*)
	     (display "] , eax " *out*)
	     (newline *out*)
	     	     
	     ;;(newline)
	     ;;(display "LET: the symbol : ") (display the-sym) (newline)
	     ;;(display "LET:the symbol body : ") (display the-sym-body) (newline)
	     ;;	     
	     (comp-let-bindings (cdr bindings)
				body
				(- si *wordsize*)
				env
				(cons (cons the-sym si) local-env)))))))



;; just close our eyes if its a begin sequence and fingers crossed
(define (comp-implicit-sequence x si env)
  (cond
   ((null? x) #f)
   (else (comp (car x) si env)
	 (comp-implicit-sequence (cdr x) si env))))




(define (comp-lookup x si env)
  (display "mov dword eax , [ esp " *out*)
  (let ((binding (assoc x env)))
    (if (pair? binding)
	(begin
	  (display (cdr binding) *out*)
	  (display "] " *out*)
	  (newline *out*))
	(begin
	  (error "comp-lookup : no binding for symbol " x)))))



;; (if cond conseq alt)
(define (comp-if x si env)
  (let ((if-condition  (car (cdr x)))
	(if-consequence (car (cdr (cdr x))))
	(if-alternate  (cdr (cdr (cdr x))))
	(false-label (gensym "if"))
	(done-label  (gensym "if")))
    
    (comp if-condition si env)
    
    ;; if false goto false
    (display "cmp dword eax , " *out*)
    (display the-false-value *out*)    
    (newline *out*)

    (display "je " *out*)
    (display false-label *out*)    
    (newline *out*)

    
    (comp if-consequence si env)
    ;; goto done label
    (display "jmp " *out*)
    (display done-label *out*)    
    (newline *out*)

    ;; false label
    (display false-label *out*)
    (display ": nop" *out*)    
    (newline *out*)

    (if (null? if-alternate)
	(begin
	  ;; if no alternate - slap in a FALSE value 
	  (comp-boolean #f si env))
	(begin
	  (set! if-alternate (car if-alternate))
	  (comp if-alternate si env)))
	
    ;; done label
    (display done-label *out*)
    (display ": nop" *out*)    
    (newline *out*)))




(define (comp-cons x si env)
  ;; (cons x y)
  ;; E[x]
  (comp (car (cdr x)) si env)
  
  ;; save onto stack
  (display "mov dword [ esp " *out*)
  (display si *out*)
  (display "] , eax " *out*)
  (newline *out*)
  
  ;; E[y]
  (comp (car (cdr (cdr x))) (- si *wordsize*) env)
  
  ;; store CDR
  (display "mov dword [ esi + 4 ] , eax " *out*)
  (newline *out*)

  (display "mov dword eax , [ esp " *out*)
  (display si *out*)
  (display "] " *out*)    
  (newline *out*)

  ;; store CAR
  (display "mov dword [ esi ] , eax " *out*)
  (newline *out*)

  ;; tag result
  (display "mov dword eax , esi " *out*)
  (newline *out*)
  
  ;; xxx001 is a PAIR tag
  (display "inc dword eax" *out*)
  (newline *out*)

  ;; bump esi
  (display "add dword esi , " *out*)
  (display (* 2 *wordsize*) *out*)
  (newline *out*))






(define (comp-car x si env)
  ;; (car x)
  ;; E[x]
  (comp (car (cdr x)) si env)

  (display "mov dword eax , [ eax - 1 ] " *out*)
  (newline *out*))


(define (comp-cdr x si env)
  ;; (cdr x)
  ;; E[x]
  (comp (car (cdr x)) si env)
  
  (display "mov dword eax , [ eax + 3 ] " *out*)
  (newline *out*))




;; default to false value
;; (make-vector 3) => #[#f #f #f]
(define (comp-make-vector x si env)
  ;; (make-vector size)
  
  ;; E[size]
  (comp (car (cdr x)) si env)
    
  ;; size
  (display "mov dword [ esi ] , eax " *out*)
  (newline *out*)

  (display "mov dword [ esi + 4 ] , " *out*)
  (display the-false-value *out*)
  (newline *out*)
  
  ;; size in EBX tagged
  (display "mov dword ebx , eax " *out*)
  (newline *out*)

  ;; size in EBX un tagged
  (display "shr dword ebx , 2" *out*)
  (newline *out*)

  ;; margin just add 2 , so if greater than 2 then keep going
  (display "add dword ebx , 4" *out*)
  (newline *out*)

  
  ;; the result in EAX
  (display "mov dword eax , esi " *out*)
  (newline *out*)

  (display "add dword eax , 2 " *out*)
  (newline *out*)
  
  (display "push dword eax" *out*)
  (newline *out*)

  ;; bump 
  (display "add dword esi , " *out*)
  (display (* 2 *wordsize*) *out*)
  (newline *out*)
  
  ;; bump
  (let ((bump-label (gensym "bump")))
    (display bump-label *out*)
    (display ": " *out*)

    (display "mov dword [ esi ] , " *out*)
    (display the-false-value *out*)
    (newline *out*)

    (display "mov dword [ esi + 4 ] , " *out*)
    (display the-false-value *out*)
    (newline *out*)
    
    (display "add dword esi , " *out*)
    (display (* 2 *wordsize*) *out*)
    (newline *out*)

    (display "dec dword ebx " *out*)
    (newline *out*)    
    (display "dec dword ebx " *out*)
    (newline *out*)    
    
    (display "cmp dword ebx , 2 " *out*)
    (newline *out*)
    (display "ja " *out*)
    (display bump-label *out*)
    (newline *out*))
  
  ;;(display "pop dword ebx" *out*)
  ;;(newline *out*)
  
  (display "pop dword eax" *out*)
  (newline *out*))













(define (comp x si env)
  (cond
   ((symbol? x) (comp-lookup x si env))
   ((null? x) (comp-null x si env))
   ((boolean? x)  (comp-boolean x si env))
   ((char? x) (comp-char x si env))    
   ((integer? x) (comp-integer x si env))
   ((and (pair? x) (eq? (car x) 'add1)) (comp-add1 x si env))  
   ((and (pair? x) (eq? (car x) 'sub1)) (comp-sub1 x si env))   
   ((and (pair? x) (eq? (car x) 'integer->char)) (comp-integer->char x si env))
   ((and (pair? x) (eq? (car x) 'char->integer)) (comp-char->integer x si env))
   ((and (pair? x) (eq? (car x) 'zero?)) (comp-zero? x si env))
   ((and (pair? x) (eq? (car x) 'null?)) (comp-null? x si env))
   ((and (pair? x) (eq? (car x) 'not)) (comp-not x si env))
   ((and (pair? x) (eq? (car x) 'boolean?)) (comp-boolean? x si env))
   ((and (pair? x) (eq? (car x) 'integer?)) (comp-integer? x si env))   
   ((and (pair? x) (eq? (car x) '+)) (comp-add x si env))
   ((and (pair? x) (eq? (car x) '-)) (comp-sub x si env))
   ((and (pair? x) (eq? (car x) 'let)) (comp-let x si env))
   ((and (pair? x) (eq? (car x) 'if)) (comp-if x si env))   
   ((and (pair? x) (eq? (car x) 'cons)) (comp-cons x si env))
   ((and (pair? x) (eq? (car x) 'car)) (comp-car x si env))   
   ((and (pair? x) (eq? (car x) 'cdr)) (comp-cdr x si env))   
   ;;((and (pair? x) (eq? (car x) 'make-string)) (comp-make-string x si env))
   ((and (pair? x) (eq? (car x) 'make-vector)) (comp-make-vector x si env))
   ;;((and (pair? x) (eq? (car x) 'vector-ref)) (comp-vector-ref x si env))
   ;;((and (pair? x) (eq? (car x) 'vector-set!)) (comp-vector-set! x si env))
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
	    
	    (display "scheme_entry: nop " *out*)
	    (newline *out*)

	    ;; HEAP is passed as 1st argument
	    (display "mov dword esi , [ esp + 4 ] " *out*)
	    (newline *out*)
	    
	    
	    ;; here compile the expression
	    ;; initial stack index is negative wordsize
	    ;; as [ esp - 4 ] , since esp holds return address.
	    (let ((initial-environment '())
		  (stack-index (- *wordsize*)))
	      (comp expr stack-index initial-environment))
	    
	    (display "ret" *out*)
	    (newline *out*)
	    
	    (newline *out*)
	    (newline *out*)
	    ))))))
























