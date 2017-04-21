
;; assume input is a closed source file
;; read in definitions
;; last expression is one that fires it off ??
;; define = toplevel definition

;; ignoring letrecs for now
;; internal define is a letrec which is let + set!


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
  ;; dont need map here , can just iterate over args
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
    (emit "mov dword [ esp "  si "] , eax ")
    ;; E[y]
    (comp (car (cdr (cdr x))) (- si *wordsize*) env)
    (emit "sub dword [ esp " si "] , eax ")
    (emit "mov dword eax , [ esp " si "] "))



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





(define (comp-mul x si env)
  ;; (* x y)
  ;; E[x]
  (comp (car (cdr x)) si env)
  ;; save onto stack
  (emit "mov dword [ esp " si "] , eax ")
  ;; E[y]
  (comp (car (cdr (cdr x))) (- si *wordsize*) env)

  (emit "shr dword eax , 2 ")
  (emit "mov dword ebx , [esp " si "]")
  (emit "shr dword ebx , 2 ")
  (emit "mul dword ebx")
  (emit "shl dword eax , 2 "))











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
  ;; (let ((done-label (gensym "done"))
  ;; 	(false-label (gensym "false"))
  ;; 	(true-label (gensym "true")))
    
  ;; (= x y)
  ;; E[x]
  (comp (car (cdr x)) si env)
  ;; save onto stack
  (emit "mov dword [ esp " si "] , eax ")
  ;; E[y]
  (comp (car (cdr (cdr x))) (- si *wordsize*) env)  
  ;; are they equal?
  (emit "cmp dword [ esp " si "] , eax ")

  (emit "mov dword eax , 0 ")
  (emit "setg al")
  (emit "shl dword eax , " primitive-boolean-shift)
  (emit "or dword eax , " (primitive-boolean-tag)))
  
  ;; (emit "jg " true-label)
  ;; (emit "mov dword eax , " the-false-value)
  ;; (emit "jmp " done-label)  
  
  ;; (emit true-label ": mov dword eax , " the-true-value)  
  ;; (emit done-label ": nop")))








(define (comp-num< x si env)
  ;; E[x]
  (comp (car (cdr x)) si env)
  ;; save onto stack
  (emit "mov dword [ esp " si "] , eax ")
  ;; E[y]
  (comp (car (cdr (cdr x))) (- si *wordsize*) env)  
  ;; are they equal?
  (emit "cmp dword [ esp " si "] , eax ")

  (emit "mov dword eax , 0 ")
  (emit "setl al")
  (emit "shl dword eax , " primitive-boolean-shift)
  (emit "or dword eax , " (primitive-boolean-tag)))




(define (comp-num<= x si env)
  ;; E[x]
  (comp (car (cdr x)) si env)
  ;; save onto stack
  (emit "mov dword [ esp " si "] , eax ")
  ;; E[y]
  (comp (car (cdr (cdr x))) (- si *wordsize*) env)  
  ;; are they equal?
  (emit "cmp dword [ esp " si "] , eax ")

  (emit "mov dword eax , 0 ")
  (emit "setle al")
  (emit "shl dword eax , " primitive-boolean-shift)
  (emit "or dword eax , " (primitive-boolean-tag)))




(define (comp-num>= x si env)
  ;; E[x]
  (comp (car (cdr x)) si env)
  ;; save onto stack
  (emit "mov dword [ esp " si "] , eax ")
  ;; E[y]
  (comp (car (cdr (cdr x))) (- si *wordsize*) env)  
  ;; are they equal?
  (emit "cmp dword [ esp " si "] , eax ")

  (emit "mov dword eax , 0 ")
  (emit "setge al")
  (emit "shl dword eax , " primitive-boolean-shift)
  (emit "or dword eax , " (primitive-boolean-tag)))


(define (comp-begin x si env)
  (cond   
   ((null? (cdr x))
    (emit "mov dword eax , " the-false-value))
   (else
    (begin
      (comp (car (cdr x)) si env)
      (comp-implicit-sequence (cdr (cdr x)) si env)))))




;; (tak x y z)
;; (if (< y x)
;;      (tak (tak (- x 1) y z)
;;           (tak (- y 1) z x)
;;           (tak (- z 1) x y))
;;      z)
(define (comp-f3x-def x si env)
  ;; E[x]
  (emit "f3x: nop")
  (comp 'x
	si
	(append '((x . -4)(y . -8)(z . -12)) env))
  (emit "ret"))


(define (comp-f3y-def x si env)
  ;; E[x]
  (emit "f3y: nop")
  (comp 'y
	si
	(append '((x . -4)(y . -8)(z . -12)) env))
  (emit "ret"))


(define (comp-f3z-def x si env)
  ;; E[x]
  (emit "f3z: nop")
  (comp 'z
	si
	(append '((x . -4)(y . -8)(z . -12)) env))
  (emit "ret"))


(define (comp-f3x x si env)
  ;; E[x]  si - 4
  (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
    ;; save onto stack
  (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ")
  
  ;; E[y]  si - 8 
  (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
  ;; save onto stack
  (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ")
  
  ;; E[z]  si - 12
  (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
  ;; save onto stack
  (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ")

  ;; adjust ESP , si is negative multiple of 4 :  -4 , -8 , -12 , -16, -20
  (emit "add dword esp , " (+ si 4))

  ;; do the CALL
  (emit "call f3x")

  ;; 
  (emit "add dword esp , " (- (+ si 4))))




(define (comp-f3y x si env)
  ;; E[x]  si - 4
  (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
    ;; save onto stack
  (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ")
  
  ;; E[y]  si - 8 
  (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
  ;; save onto stack
  (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ")
  
  ;; E[z]  si - 12
  (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
  ;; save onto stack
  (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ")

  ;; adjust ESP , si is negative multiple of 4 :  -4 , -8 , -12 , -16, -20
  (emit "add dword esp , " (+ si 4))

  ;; do the CALL
  (emit "call f3y")

  ;; 
  (emit "add dword esp , " (- (+ si 4))))


(define (comp-f3z x si env)
  ;; E[x]  si - 4
  (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
    ;; save onto stack
  (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ")
  
  ;; E[y]  si - 8 
  (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
  ;; save onto stack
  (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ")
  
  ;; E[z]  si - 12
  (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
  ;; save onto stack
  (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ")

  ;; adjust ESP , si is negative multiple of 4 :  -4 , -8 , -12 , -16, -20
  (emit "add dword esp , " (+ si 4))

  ;; do the CALL
  (emit "call f3z")

  ;; 
  (emit "add dword esp , " (- (+ si 4))))




(define (comp-fib-def x si env)
  ;; E[x]
  (emit "fib: nop")
  (comp '(if (= n 1)
	     1
	     (if (= n 2)
		 1
		 (+ (fib (- n 1))
		    (fib (- n 2)))))
	;;  1 arg = 2 wordsizes 
  	(- (* 2 *wordsize*))
  	(append '((n . -4)) env))
  (emit "ret"))







(define (comp-fac-def x si env)
  ;; E[x]
  (emit "fac: nop")
  (comp '(if (= n 1)
	     1
	     (* n (fac (- n 1))))
	;;  1 arg = 2 wordsizes  huh.
  	(- (* 2 *wordsize*))
  	(append '((n . -4)) env))
  (emit "ret"))



(define (comp-tak-def x si env)
  ;; E[x]
  (emit "")
  (emit "tak1: nop")
  (emit "      jmp tak")
  (emit "")  
  (emit "tak2: nop")
  (emit "      jmp tak")
  (emit "")    
  (emit "tak3: nop")
  (emit "      jmp tak")
  (emit "")
  (emit "tak4: nop")
  (emit "      jmp tak")
  (emit "")
  
  (emit "tak: nop")
  (comp '(begin
	   ;;(debug-tak)
	   (if (< y x)
	       (tak4 (tak1 (- x 1) y z)
		     (tak2 (- y 1) z x)
		     (tak3 (- z 1) x y))
	       z))
	;; 3 args = 4 wordsize huh.
  	(- (* 4 *wordsize*))
  	(append '((x . -4)(y . -8)(z . -12)) env))
  (emit "ret"))


(define (comp-tak x si env)
  ;; (TAK x y z)
  ;; si = keep empty for ESP to go into.
  ;;(emit "nop ; starts TAK here")
  ;; E[x]  si - 4
  (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
  (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ; save X ")
  ;; E[y]  si - 8 
  (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
  (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ; save Y")
  ;; E[z]  si - 12
  (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
  (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ; save Z" )
  (emit "add dword esp , " (+ si *wordsize*) "; adjust esp")
  (emit "call tak ; " (- si (* 3 *wordsize*)))
  (emit "sub dword esp , " (+ si *wordsize*) "; restore esp"))

(define (comp-tak-tail-recursive x si env)
  ;; (TAK x y z)
  ;; si = keep empty for ESP to go into.
  ;;(emit "nop ; starts TAK here")
  ;; E[x]  si - 4
  (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
  (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ; save X ")
  ;; E[y]  si - 8 
  (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
  (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ; save Y")
  ;; E[z]  si - 12
  (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
  (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ; save Z" )
  (emit "nop ; collapse arguments by " si)
  
  (emit "mov dword eax , [esp  " (- si (* 1 *wordsize*)) "] ; collapse X ")
  (emit "mov dword [esp  " (- (* 1 *wordsize*)) "] , eax ")

  (emit "mov dword eax , [esp  " (- si (* 2 *wordsize*)) "] ; collapse Y")
  (emit "mov dword [esp  " (- (* 2 *wordsize*)) "] , eax ")

  (emit "mov dword eax , [esp  " (- si (* 3 *wordsize*)) "] ; collapse Z")
  (emit "mov dword [esp  " (- (* 3 *wordsize*)) "] , eax ")
  
  ;;(emit "add dword esp , " (+ si *wordsize*) "; adjust esp")
  (emit "jmp tak4 ; allow for gdb to record ARGS to TAK")
  ;;(emit "sub dword esp , " (+ si *wordsize*) "; restore esp")
  )





(define (comp-tak-generic nth-tak x si env)
  (cond
   ((= nth-tak 4) ;; TAK4 is tail recursive
    (comp-tak-tail-recursive x si env))
   ;;(#f #f)
   (else
    (begin
      ;; (TAK x y z)
      ;; si = keep empty for ESP to go into.
      ;;(emit "nop ; starts TAK here")
      ;; E[x]  si - 4
      (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
      (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ; save X ")
      ;; E[y]  si - 8 
      (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
      (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ; save Y")
      ;; E[z]  si - 12
      (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
      (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ; save Z" )
      (emit "add dword esp , " (+ si *wordsize*) "; adjust esp")
      (emit "call tak" nth-tak " ; " (- si (* 3 *wordsize*)))
      (emit "sub dword esp , " (+ si *wordsize*) "; restore esp")))))


;; this debug code is not used
(define (comp-debug-tak x si env)
  (emit "pushad")
  (emit "push dword esp")
  (emit "call debug_stack")
  (emit "add dword esp , 4")
  (emit "popad"))










(define (comp-fib x si env)
  ;; (FIB n)

  ;; si = keep empty for ESP to go into.
  ;;(emit "nop ; FIB call now ")
  
  ;; E[x]  si - 4
  (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
  
    ;; save onto stack
  (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ")
  
  ;; ;; E[y]  si - 8 
  ;; (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
  ;; ;; save onto stack
  ;; (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ")
  
  ;; ;; E[z]  si - 12
  ;; (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
  ;; ;; save onto stack
  ;; (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ")

  ;; adjust ESP , si is negative multiple of 4 :  -4 , -8 , -12 , -16, -20
  (emit "add dword esp , " (+ si *wordsize*))

  ;; do the CALL
  (emit "call fib")
  ;; 
  (emit "sub dword esp , " (+ si *wordsize*)))

;;(- (+ si 4))))









(define (comp-fac x si env)
  ;; (FAC n)

  ;; si = keep empty for ESP to go into.
  ;;(emit "nop ; FIB call now ")
  
  ;; E[x]  si - 4
  (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
  
    ;; save onto stack
  (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ")
  
  ;; ;; E[y]  si - 8 
  ;; (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
  ;; ;; save onto stack
  ;; (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ")
  
  ;; ;; E[z]  si - 12
  ;; (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
  ;; ;; save onto stack
  ;; (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ")

  ;; adjust ESP , si is negative multiple of 4 :  -4 , -8 , -12 , -16, -20
  (emit "add dword esp , " (+ si *wordsize*))

  ;; do the CALL
  (emit "call fac")
  ;; 
  (emit "sub dword esp , " (+ si *wordsize*)))



;; define 
(define (comp-define x si env)
  (let ((var (car (cdr x)))
	(val (car (cdr (cdr x)))))
    ;; compile the val

    ;; assign it to var
    #f
    ;; var now bound to val in toplevel environment
    ))











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
   ((and (pair? x) (eq? (car x) '*)) (comp-mul x si env))
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
   ((and (pair? x) (eq? (car x) '<=)) (comp-num<= x si env))
   ((and (pair? x) (eq? (car x) '>=)) (comp-num>= x si env))

   ;; define
   ((and (pair? x) (eq? (car x) 'define)) (comp-define x si env))

   ;; begin
   ((and (pair? x) (eq? (car x) 'begin)) (comp-begin x si env))

   ;; tak 
   ((and (pair? x) (eq? (car x) 'tak)) (comp-tak x si env))
   
   ((and (pair? x) (eq? (car x) 'tak1)) (comp-tak-generic 1 x si env))
   ((and (pair? x) (eq? (car x) 'tak2)) (comp-tak-generic 2 x si env))
   ((and (pair? x) (eq? (car x) 'tak3)) (comp-tak-generic 3 x si env))
   ((and (pair? x) (eq? (car x) 'tak4)) (comp-tak-generic 4 x si env))

   ;; f3[xyz] x y z
   ((and (pair? x) (eq? (car x) 'f3x)) (comp-f3x x si env))
   ((and (pair? x) (eq? (car x) 'f3y)) (comp-f3y x si env))
   ((and (pair? x) (eq? (car x) 'f3z)) (comp-f3z x si env))

   ;; fib
   ((and (pair? x) (eq? (car x) 'fib)) (comp-fib x si env))

   ;; factorial fac
   ((and (pair? x) (eq? (car x) 'fac)) (comp-fac x si env))

   ;; debug-tak
   ((and (pair? x) (eq? (car x) 'debug-tak)) (comp-debug-tak x si env))

   (else #f)))


(define (set-emit-output-port! p)
  (set! *out* p))





(define (compile-program input output)
  (call-with-input-file input
    (lambda (in-port)
      (let ((expr (read in-port)))
	(call-with-output-file output
	  (lambda (port)
	    (set-emit-output-port! port)
	    
	    (emit "")
	    (emit "extern debug_stack")
	    (emit "global scheme_entry")
	    
	    ;; here compile the expression
	    ;; initial stack index is negative wordsize
	    ;; as [ esp - 4 ] , since esp holds return address.
	    (let ((initial-environment '())
		  (stack-index (- *wordsize*)))

	      ;;(comp-tak-def #f stack-index initial-environment)

	      ;; (comp-fib-def #f stack-index initial-environment)
	      ;; (comp-fac-def #f stack-index initial-environment)
	      ;; (comp-f3x-def #f stack-index initial-environment)
	      ;; (comp-f3y-def #f stack-index initial-environment)
	      ;; (comp-f3z-def #f stack-index initial-environment)
	      
	      (emit "scheme_entry: nop ")	      
	      ;; HEAP is passed as 1st argument
	      (emit "mov dword esi , [ esp + 4 ] ")
	      (emit "scheme_heap_in_esi: nop")
	      
	      (comp expr stack-index initial-environment)	      
	      (emit "ret"))
	    
	    (emit "")
	    (emit "")
	    (emit "")))))))










	    























