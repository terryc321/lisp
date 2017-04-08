



;; on 32bit system *wordsize* is 4 , as 4 bytes 4 x 8bits per byte = 32 bits in total
;; on 64bit system *wordsize* is 8 , as 8 bytes 8 x 8bits per byte = 64 bits in total
(define *wordsize*  4)


(define *out* (current-output-port))

(define the-empty-list-value 47)

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
(define the-false-value 31)
(define the-true-value 159)

(define the-boolean-mask
  (+ 64 32 16 8 4 2 1))


(define the-integer-mask
  (+ 1 2))

(define the-integer-tag 0)


;;(+ 1 2 4 8 16) #f
;;(+ 1 2 4 8 16 128) #t

;;; Important to remember these are the PRIMITIVE routines of the system 
;;; Therefore are not subject to evaluator , they do their stuff at machine level

(define (emit . args)
  (map (lambda (x) (display x *out*)) args)
  (newline *out*))



(define (comp-null x si env)
  (emit "mov dword eax , " the-empty-list-value))


(define (comp-boolean x si env)
  (if x 
      (begin ;; true
	(emit "mov dword eax , " the-true-value))
      (begin ;; false
	(emit "mov dword eax , " the-false-value))))


(define (comp-char x si env)
  (emit "mov dword eax , "
	(+ (shift-left (char->integer x) primitive-character-shift)
	   (primitive-character-tag))))

(define (comp-integer x si env)
  (emit "mov dword eax , " 
	(shift-left x fixnum-shift)))

(define (comp-add1 x si env)
  (comp (car (cdr x)) si env)
  (emit "add dword eax , " 
	(shift-left 1 fixnum-shift)))

(define (comp-sub1 x si env)
  (comp (car (cdr x)) si env)
  (emit "sub dword eax , " 
	(shift-left 1 fixnum-shift)))

(define (comp-integer->char x si env)
  (comp (car (cdr x)) si env)
  (emit "shl dword eax , 6")
  (emit "add dword eax , " (primitive-character-tag)))


(define (comp-char->integer x si env)
  (comp (car (cdr x)) si env)
  (emit "shr dword eax , 6"))



(define (comp-zero? x si env)
  (comp (car (cdr x)) si env)
  (emit "cmp dword eax , 0")
  (emit "mov dword eax , 0")
  (emit "sete al")
  (emit "shl dword eax , " primitive-boolean-shift)
  (emit "or dword eax , " (primitive-boolean-tag)))



(define (comp-null? x si env)
  (comp (car (cdr x)) si env)
  (emit "cmp dword eax , " the-empty-list-value)
  (emit "mov dword eax , 0")
  (emit "sete al")
  (emit "shl dword eax , " primitive-boolean-shift)
  (emit "or dword eax , " (primitive-boolean-tag)))




(define (comp-not x si env)
  (comp (car (cdr x)) si env)  
  (emit "cmp dword eax , " the-false-value)
  (emit "mov dword eax , 0")
  (emit "sete al" )  
  (emit "shl dword eax , " primitive-boolean-shift)
  (emit "or dword eax , " (primitive-boolean-tag)))



(define (comp-boolean? x si env)
  (comp (car (cdr x)) si env)  
  (emit "and dword eax , " the-boolean-mask)
  (emit "cmp dword eax , " the-boolean-tag)
  (emit "mov dword eax , 0")
  (emit "sete al")
  (emit "shl dword eax , " primitive-boolean-shift)
  (emit "or dword eax , "  (primitive-boolean-tag)))



(define (comp-integer? x si env)
  (comp (car (cdr x)) si env)
  (emit "and dword eax , " the-integer-mask)
  (emit "cmp dword eax , " the-integer-tag)
  (emit "mov dword eax , 0")
  (emit "sete al" )   
  (emit "shl dword eax , " primitive-boolean-shift)
  (emit "or dword eax , " (primitive-boolean-tag)))


(define (comp-add x si env)
  (comp (car (cdr x)) si env)
  ;; save arg onto stack
  (emit "mov dword [ esp " si "] , eax ")   
  ;; E[y]
  (comp (car (cdr (cdr x))) (- si *wordsize*) env)
  (emit "add dword eax , [ esp " si "] "))




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
	     (emit "mov dword [ esp " si "] , eax ")
	     	     
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
  (let ((binding (assoc x env)))
    (if (pair? binding)
	(begin
	  (emit "mov dword eax , [ esp " (cdr binding)  "] "))
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
    (emit "cmp dword eax , " the-false-value)

    (emit "je " false-label)
    
    (comp if-consequence si env)
    ;; goto done label
    (emit "jmp " done-label)

    ;; false label
    (emit false-label ": nop")

    (if (null? if-alternate)
	(begin
	  ;; if no alternate - slap in a FALSE value 
	  (comp-boolean #f si env))
	(begin
	  (set! if-alternate (car if-alternate))
	  (comp if-alternate si env)))
	
    ;; done label
    (emit done-label ": nop")))




(define (comp-cons x si env)
  ;; (cons x y)
  ;; E[x]
  (comp (car (cdr x)) si env)
  ;; save onto stack
  (emit "mov dword [ esp " si "] , eax ")
  ;; E[y]
  (comp (car (cdr (cdr x))) (- si *wordsize*) env)  
  ;; store CDR
  (emit "mov dword [ esi + 4 ] , eax ")
  (emit "mov dword eax , [ esp " si "] ")
  ;; store CAR
  (emit "mov dword [ esi ] , eax ")
  ;; tag result
  (emit "mov dword eax , esi ")
  ;; xxx001 is a PAIR tag
  (emit "inc dword eax")
  ;; bump esi
  (emit "add dword esi , "  (* 2 *wordsize*)))



(define (comp-car x si env)
  ;; (car x)
  ;; E[x]
  (comp (car (cdr x)) si env)
  (emit "mov dword eax , [ eax - 1 ] "))


(define (comp-cdr x si env)
  ;; (cdr x)
  ;; E[x]
  (comp (car (cdr x)) si env)
  (emit "mov dword eax , [ eax + 3 ] "))




;; default to false value
;; (make-vector 3) => #[#f #f #f]
(define (comp-make-vector x si env)
  ;; (make-vector size)
  
  ;; E[size]
  (comp (car (cdr x)) si env)
    
  ;; size
  (emit "mov dword [ esi ] , eax ")
  (emit "mov dword [ esi + 4 ] , " the-false-value)
  ;; size in EBX tagged
  (emit "mov dword ebx , eax ")
  ;; size in EBX un tagged
  (emit "shr dword ebx , 2")
  ;; margin just add 2 , so if greater than 2 then keep going
  (emit "add dword ebx , 4")
  ;; the result in EAX
  (emit "mov dword eax , esi ")
  (emit "add dword eax , 2 ")
  (emit "push dword eax")
  ;; bump 
  (emit "add dword esi , " (* 2 *wordsize*))
  
  ;; bump
  (let ((bump-label (gensym "bump")))
    (emit bump-label ": mov dword [ esi ] , " the-false-value)
    (emit "mov dword [ esi + 4 ] , " the-false-value)
    (emit "add dword esi , " (* 2 *wordsize*))
    (emit "dec dword ebx ")    
    (emit "dec dword ebx ")    
    (emit "cmp dword ebx , 2 ") 
    (emit "ja " bump-label))
  
  (emit "pop dword eax"))




(define (comp-odd? x si env)
  (comp (car (cdr x)) si env)
  (emit "and dword eax , 0b0100")
  (emit "shl dword eax , 5")
  (emit "or dword eax , 0b11111"))


(define (comp-even? x si env)
  (comp (car (cdr x)) si env)
  (emit "xor dword eax , 0b0100")  
  (emit "and dword eax , 0b0100")  
  (emit "shl dword eax , 5")
  (emit "or dword eax , 0b11111"))




;; ff -1     11
;; fe -2     10
;; fd -3     01
;; fc -4     00

(define (comp-div2 x si env)
  (comp (car (cdr x)) si env)
  (emit "shr dword eax , 3 ")
  (emit "shl dword eax , 2"))

  
  ;; (emit "nop")
  ;; (emit "shr dword eax , 3 ")
  ;; (emit "clc")  
  ;; (emit "shl dword eax , 2"))

;; (emit "mov dword ebx , 0x7ffffffc")
;;   (emit "and dword eax , ebx")
;;   (emit "shr dword eax , 1 ")
;;   (emit "and al , 0xfc ")
;;   )
  ;;(emit "shl dword eax , 2"))


;; multiply EAX by ?
;; the result ends in EDX : EAX
;; lower 2 bits zero 0 0  = means number tag
;; multiply by 3 then add 1
;;  1 tagged is number 4
;;      1 0 0
(define (comp-mul3+1 x si env)
  (comp (car (cdr x)) si env)
  (emit "mov dword ebx , 3")  
  (emit "mul dword ebx")
  (emit "add dword eax , 4"))




(define (comp-collatz x si env)
  (emit "global collatz")
  (emit "collatz: nop ")
  ;;(emit "cmp dword [esp + 4]")
  )





(define (comp-num= x si env)
  ;; (= x y)
  ;; E[x]
  (comp (car (cdr x)) si env)
  ;; save onto stack
  (emit "mov dword [ esp " si "] , eax ")
  ;; E[y]
  (comp (car (cdr (cdr x))) (- si *wordsize*) env)  
  ;; are they equal?
  (emit "cmp dword [ esp " si "] , eax ")
  ;; 
  (emit "mov dword eax , 0 ")
  (emit "sete al")
  (emit "shl dword eax , " primitive-boolean-shift)
  (emit "or dword eax , " (primitive-boolean-tag)))
  
  
  ;; (emit "mov dword [ esi + 4 ] , eax ")
  ;; (emit "mov dword eax , [ esp " si "] ")
  ;; ;; store CAR
  ;; (emit "mov dword [ esi ] , eax ")
  ;; ;; tag result
  ;; (emit "mov dword eax , esi ")
  ;; ;; xxx001 is a PAIR tag
  ;; (emit "inc dword eax")
  ;; ;; bump esi
  ;; (emit "add dword esi , "  (* 2 *wordsize*)))




;; 
(define (comp-num> x si env)
  (let ((done-label (gensym "done"))
	(false-label (gensym "false"))
	(true-label (gensym "true")))
    
  ;; (= x y)
  ;; E[x]
  (comp (car (cdr x)) si env)
  ;; save onto stack
  (emit "mov dword [ esp " si "] , eax ")
  ;; E[y]
  (comp (car (cdr (cdr x))) (- si *wordsize*) env)  
  ;; are they equal?
  (emit "cmp dword [ esp " si "] , eax ")

  (emit "jg " true-label)
  (emit "mov dword eax , " the-false-value)
  (emit "jmp " done-label)  
  
  (emit true-label ": mov dword eax , " the-true-value)  
  (emit done-label ": nop")))



(define (comp-num< x si env)
  (let ((done-label (gensym "done"))
	(false-label (gensym "false"))
	(true-label (gensym "true")))
    
  ;; (= x y)
  ;; E[x]
  (comp (car (cdr x)) si env)
  ;; save onto stack
  (emit "mov dword [ esp " si "] , eax ")
  ;; E[y]
  (comp (car (cdr (cdr x))) (- si *wordsize*) env)  
  ;; are they equal?
  (emit "cmp dword [ esp " si "] , eax ")

  (emit "jl " true-label)
  (emit "mov dword eax , " the-false-value)
  (emit "jmp " done-label)  
  
  (emit true-label ": mov dword eax , " the-true-value)  
  (emit done-label ": nop")))








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
   ((and (pair? x) (eq? (car x) 'div2)) (comp-div2 x si env))  
   ((and (pair? x) (eq? (car x) 'mul3+1)) (comp-mul3+1 x si env))

   ((and (pair? x) (eq? (car x) 'odd?)) (comp-odd? x si env))
   ((and (pair? x) (eq? (car x) 'even?)) (comp-even? x si env))

   ;; fixnum comparisons
   ((and (pair? x) (eq? (car x) '=)) (comp-num= x si env))
   ;; 
   ((and (pair? x) (eq? (car x) '>)) (comp-num> x si env))
   ((and (pair? x) (eq? (car x) '<)) (comp-num< x si env))
   ;; ((and (pair? x) (eq? (car x) '<=)) (comp-num<= x si env))
   ;; ((and (pair? x) (eq? (car x) '>=)) (comp-num>= x si env))
   
   (else #f)))









(define (compile-program input output)
  (call-with-input-file input
    (lambda (in-port)
      (let ((expr (read in-port)))
	(call-with-output-file output
	  (lambda (port)
	    (set! *out* port)
	    
	    (emit "")
	    (emit "")
	    (emit "global scheme_entry")
	    (emit "scheme_entry: nop ")

	    ;; HEAP is passed as 1st argument
	    (emit "mov dword esi , [ esp + 4 ] ")
	    
	    ;; here compile the expression
	    ;; initial stack index is negative wordsize
	    ;; as [ esp - 4 ] , since esp holds return address.
	    (let ((initial-environment '())
		  (stack-index (- *wordsize*)))
	      (comp expr stack-index initial-environment))
	    
	    (emit "ret")
	    (emit "")
	    (emit "")
	    (emit "")))))))


	    























