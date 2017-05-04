
;; tidy proc name , removes minus sign from
;;


;; assume input is a closed source file
;; read in definitions
;; last expression is one that fires it off ??
;; define = toplevel definition

;; ignoring letrecs for now
;; internal define is a letrec which is let + set!

;; closure uses HEAP
;; vector uses HEAP

;; on 32bit system *wordsize* is 4 , as 4 bytes 4 x 8bits per byte = 32 bits in total
;; on 64bit system *wordsize* is 8 , as 8 bytes 8 x 8bits per byte = 64 bits in total
(define word  4)

(define *wordsize*  4)


(define *primitives*
  '(add1
    sub1
    integer->char
    char->integer
    zero?
    null?
    boolean?
    integer?
    *
    +
    -
    let
    if
    cons
    car
    cdr
    make-vector
    div2
    mul3+1
    odd?
    even?
    =
    >
    <
    <=
    >=
    define
    begin
    lambda
    quote
    not
    tailcall
    #t
    #f
    ))


;; binding forms are
;; comp-let-bindings  (list the-sym 'local  si)
;; comp-lookup 




;; only allow a-z A-Z 0-9
(define (tidy-proc-name symbol)
  (apply string
	 (append (list #\f #\n #\_ )
		 (map (lambda (x)
			(cond
			 ((char=? x #\-) #\_)
			 ((char=? x #\_) #\_)
			 ((and (char>=? x #\a) (char<=? x #\z)) x)
			 ((and (char>=? x #\A) (char<=? x #\Z)) x)
			 ((and (char>=? x #\0) (char<=? x #\9)) x)
			 (else (error "comp.scm: tidy-proc-name : "
				      "symbol contains non-alpha characters" symbol))))
		      (string->list (symbol->string symbol))))))


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


(define (emit-align-heap-pointer)
  (emit "add dword eax , 8")
  (emit "and dword eax , (-8) "))


;; ********************************************************************************
;; si : stack index represents the free slot on the stack
;;  it is perfectly fine to assign results to SI slot 
;;  
;; does the SI register increase or decrease ??
;;

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
	(shift-left x fixnum-shift)
	" ; integer " x))


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




(define (comp-car x si env)
  (let ((arg1 (car (cdr x))))
    (comp arg1 si env)
    (emit "mov dword eax , [ eax - 1 ] ")))

(define (comp-cdr x si env)
  (let ((arg1 (car (cdr x))))
    (comp arg1 si env)
    (emit "mov dword eax , [ eax + 3 ] ")))


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







;; just close our eyes if its a begin sequence and fingers crossed
(define (comp-implicit-sequence x si env)
  (cond
   ((null? x) #f)
   (else (comp (car x) si env)
	 (comp-implicit-sequence (cdr x) si env))))


;;*******************************************************************



(define (comp-cons x si env)
  (let ((arg1 (car (cdr x)))
	(arg2 (car (cdr (cdr x)))))
    
    
    ;; compile arg1
    (comp arg1 si env)
      
    ;; save onto stack
    ;;(emit "mov dword [ esp " si "] , eax ")
    (emit "push dword eax")
    
    ;;(emit "push dword eax")
    ;; compile arg2
    (comp arg2 (- si word) env) ;;(- si word) env)
            
    ;;(emit "mov dword [ esp " (- si word) "] , eax ")
    
    ;; arg2 in EAX
    ;;(emit "mov dword [ esp " b "] , eax")
    
    ;;(emit "mov dword [ esp " (- si word) "] , eax ")
    ;; heap in ESI register  
    ;;(emit "mov dword eax , [ esp " b "] ")
    
    ;; store CDR in HEAP
    (emit "mov dword [ esi + 4 ] , eax ")
    ;; load arg1 into EAX
    (emit "mov dword eax , [ esp " si " ] ") ;;" si "] ")
    
    (emit "add dword esp ,4 ")
    
    ;; store CAR in HEAP
    (emit "mov dword [ esi ] , eax ")
    ;; tag result
    (emit "mov dword eax , esi ")
    ;; xxx001 is a PAIR tag
    (emit "and dword eax , (-8) ")
    ;;(emit "inc dword eax")
    (emit "or dword eax , 001b")    
    ;; bump HEAP ESI 
    (emit "add dword esi , "  (* 2 word))))



(define (comp-add x si env)
  (let ((arg1 (car (cdr x)))
	(arg2 (car (cdr (cdr x)))))
    ;; compile arg1 
    (comp arg1 si env)
    ;; save arg onto stack
    (emit "push dword eax")
    ;;(emit "mov dword [ esp " si "] , eax ")
    
    ;; compile arg2
    (comp arg2 (- si word) env)
    ;; arg2 is in EAX 
    ;;(emit "add dword eax , [ ebp - " si " ]")
    (emit "add dword eax , [ esp ]")
    (emit "add dword esp , 4")))
    



(define (comp-sub x si env)
  (let ((arg1 (car (cdr x)))
	(arg2 (car (cdr (cdr x)))))
    ;; compile arg1
    (comp arg1 si env)
    ;; save onto stack
    ;;(emit "push dword eax")
    (emit "mov dword [ esp " si "] , eax ")    
    ;; compile arg2
    (comp arg2 (- si word) env)
    ;; arg2 in EAX
    ;;(emit "sub dword [ ebp - " si "] , eax ")
    ;;(emit "mov dword eax , [ ebp - " si "] ")
    (emit "sub dword [ esp " si "] , eax")
    (emit "mov dword eax , [ esp " si "] ")))





;; SI stack index is POSITIVE multiple of word (word = 4)
;; results end in EAX register
;; push 
;; ***********************************************************************************




;; (let ...)
(define (comp-let x si env)
  (let ((bindings (car (cdr x)))
	(body (cdr (cdr x)))
	(local-env '()))    
    (comp-let-bindings bindings body si env local-env)))


(define (comp-let-bindings bindings body si env local-env)
  (cond
   ;; extend environment with local bindings
   ;; no more bindings to do 
   ((null? bindings)
    (display "LET: new local env : ") (display local-env) (newline)    
    (comp-implicit-sequence body si (append local-env env)))
   
   ;; some more bindings to do
   (else (let ((the-binding (car bindings)))
	   (let ((the-sym (car the-binding))
		 (the-sym-body (cdr the-binding)))
	     
	     (comp-implicit-sequence the-sym-body si env)
	     ;; mov eax into si
	     ;; si represents the free slot on stack
	     ;;(emit "mov dword [ ebp " si "] , eax  ; let bound " the-sym)
	     (emit "push dword eax")
	     
	     	     
	     ;;(newline)
	     ;;(display "LET: the symbol : ") (display the-sym) (newline)
	     ;;(display "LET:the symbol body : ") (display the-sym-body) (newline)
	     ;;	     
	     (comp-let-bindings (cdr bindings)
				body
				(+ si word)
				env
				(cons (list the-sym 'local si) local-env)))))))






(define (toplevel-binding? x)
  (and (= (length x) 3)
       (eq? (car (cdr x)) 'toplevel)))


(define (local-binding? x)
  (and (= (length x) 3)
       (eq? (car (cdr x)) 'local)))


(define (closure-binding? x)
  (and (= (length x) 3)
       (eq? (car (cdr x)) 'closure)))


(define (binding-stack-index x)
  (car (cdr (cdr x))))


(define (binding-index x)
  (car (cdr (cdr x))))





;; 1 . local binding = from LET and on STACK
;; 2 . closure binding = from LAMBDA and on STACK through RAW untagged CLOSURE POINTER
;; 3 . toplevel binding = from DEFINE 
(define (comp-lookup var si env)
  (let ((binding (assoc var env)))
    (cond
     ((local-binding? binding)
      (emit "mov dword eax , [ ebp + " (binding-stack-index binding)  "] "))
     ((closure-binding? binding)
      (emit "mov dword eax , [ ebp + 8 ] ; closure ptr into eax")
      (emit "mov dword eax , [ eax + " (binding-stack-index binding) "] "))
     ((toplevel-binding? binding)
	;; toplevel definitions live in data section
	(emit "mov dword ebx , toplevel ;; toplevel define " var)
	(emit "mov dword eax , [ebx + " (binding-index binding)"]"))
     (else
      (error "comp-lookup : no binding for symbol " var)))))





















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
  (emit "or dword eax , 2 ") ;; add dword eax , 2  --- was add
  (emit "mov dword ecx , eax")
  
  ;; bump 
  (emit "add dword esi , " (* 2 word))
  
  ;; bump
  (let ((bump-label (gensym "bump")))
    (emit bump-label ": mov dword [ esi ] , " the-false-value)
    ;;(emit "mov dword [ esi + 4 ] , " the-false-value)
    ;;(emit "add dword esi , " (* 2 word))
    (emit "add dword esi , " word)
    ;;(emit "dec dword ebx ")    
    (emit "dec dword ebx ")    
    (emit "cmp dword ebx , 0 ") 
    (emit "ja " bump-label))
  ;; align heap ptr on 8 byte boundary
  ;; ie ESI is multiple of 8
  (emit-align-heap-pointer)
  
  (emit "mov dword eax, ecx"))



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


;; fixnum lower 2 bits should be zero
;; and with -4
;; -4 in twos complement is X
(define (comp-div2 x si env)
  (let ((arg1 (car (cdr x))))
    (comp arg1 si env)
    (emit "shr dword eax , 1 ")
    (emit "and dword eax , -4 ")))


  
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
  ;; 4 is 1 shifted left twice 00 is fixnum tag
  (emit "add dword eax , 4"))





(define (comp-collatz x si env)
  (emit "global collatz")
  (emit "collatz: nop ")
  ;;(emit "cmp dword [esp + 4]")
  )




;; eg 1 * 1 would be 4 * 4 in fixnum tagged form
(define (comp-mul x si env)
  (let ((arg1 (car (cdr x)))
	(arg2 (car (cdr (cdr x)))))    
    (comp arg1 si env)
    ;; save onto stack
    (emit "mov dword [ esp " si "] , eax ")
    
    ;; compile 2nd branch
    (comp arg2 (- si word) env)

    ;; arg2 in eax - untag fixnum
    (emit "shr dword eax , 2 ")

    ;; 
    (emit "mov dword ebx , [esp " si "]")
    ;; arg1 in ebx - untag fixnum
    (emit "shr dword ebx , 2 ")
    ;; multiply 
    (emit "mul dword ebx")
    ;; leaves result in edx : eax ??
    (emit "shl dword eax , 2 ")))



(define (comp-num= x si env)
  (let ((arg1 (car (cdr x)))
	(arg2 (car (cdr (cdr x)))))
    ;; compile arg1
    (comp arg1 si env)
    
    ;; save onto stack
    (emit "mov dword [ esp " si "] , eax ")
    
    ;; compile arg2
    (comp arg2  (- si word) env)
    
    ;; are they equal?
    (emit "cmp dword [ esp " si "] , eax ")

    ;; 
    (emit "mov dword eax , 0 ")
    (emit "sete al")
    (emit "shl dword eax , " primitive-boolean-shift)
    (emit "or dword eax , " (primitive-boolean-tag))))

 
  
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
  (let ((arg1 (car (cdr x)))
	(arg2 (car (cdr (cdr x)))))
    ;; compile arg1
    (comp arg1 si env)
    ;; save onto stack
    (emit "mov dword [ esp " si "] , eax ")
    
    ;; compile arg2
    (comp arg2 (- si word) env)
    
    ;; are they equal?
    (emit "cmp dword [ esp " si "] , eax ")
    
    (emit "mov dword eax , 0 ")
    (emit "setg al")
    (emit "shl dword eax , " primitive-boolean-shift)
    (emit "or dword eax , " (primitive-boolean-tag))))




(define (comp-num< x si env)
  (let ((arg1 (car (cdr x)))
	(arg2 (car (cdr (cdr x)))))
    ;; compile arg1
    (comp arg1 si env)    
    ;; save onto stack
    (emit "mov dword [ esp " si "] , eax ")

    ;; compile arg2
    (comp arg2 (- si word) env)
    ;; are they equal?
    (emit "cmp dword [ esp " si "] , eax ")

    (emit "mov dword eax , 0 ")
    (emit "setl al")
    (emit "shl dword eax , " primitive-boolean-shift)
    (emit "or dword eax , " (primitive-boolean-tag))))




(define (comp-num<= x si env)
  (let ((arg1 (car (cdr x)))
	(arg2 (car (cdr (cdr x)))))
    ;; comp arg1
  (comp arg1 si env)
  ;; save onto stack
  (emit "mov dword [ esp " si "] , eax ")
  ;; comp arg2
  (comp arg2 (- si word) env)
  ;; are they equal?
  (emit "cmp dword [ esp " si "] , eax ")

  (emit "mov dword eax , 0 ")
  (emit "setle al")
  (emit "shl dword eax , " primitive-boolean-shift)
  (emit "or dword eax , " (primitive-boolean-tag))))




(define (comp-num>= x si env)
  (let ((arg1 (car (cdr x)))
	(arg2 (car (cdr (cdr x)))))

    (comp arg1 si env)
    ;; save onto stack
    (emit "mov dword [ esp " si "] , eax ")
    
    (comp arg2 (- si word) env)
    
    ;; are they equal?
    (emit "cmp dword [ esp " si "] , eax ")

    (emit "mov dword eax , 0 ")
    (emit "setge al")
    (emit "shl dword eax , " primitive-boolean-shift)
    (emit "or dword eax , " (primitive-boolean-tag))))







(define (comp-begin x si env)
  (cond   
   ((null? (cdr x))
    (emit "mov dword eax , " the-false-value))
   (else
    (begin
      (comp (car (cdr x)) si env)
      (comp-implicit-sequence (cdr (cdr x)) si env)))))



;; assume x is a lambda 
;; define f x
;; should really be called TOPLEVEL-DEFINE
(define (comp-define x si env)
  (let ((var (car (cdr x)))
	(val (car (cdr (cdr x)))))
    ;; compile the val
    (comp val si env)
    ;; find global index for the var
    (let ((binding (assoc var env)))
      (cond
       ((toplevel-binding? binding)
	;; toplevel definitions live in data section
	(emit "mov dword ebx , toplevel ;; toplevel define " var)
	(emit "mov dword [ebx + " (binding-index binding)"] , eax"))
       (else
	(error "comp-define : no SLOT for TOPLEVEL DEFINE found " var))))))






;; ;; (tak x y z)
;; ;; (if (< y x)
;; ;;      (tak (tak (- x 1) y z)
;; ;;           (tak (- y 1) z x)
;; ;;           (tak (- z 1) x y))
;; ;;      z)
;; (define (comp-f3x-def x si env)
;;   ;; E[x]
;;   (emit "f3x: nop")
;;   (comp 'x
;; 	si
;; 	(append '((x . -4)(y . -8)(z . -12)) env))
;;   (emit "ret"))


;; (define (comp-f3y-def x si env)
;;   ;; E[x]
;;   (emit "f3y: nop")
;;   (comp 'y
;; 	si
;; 	(append '((x . -4)(y . -8)(z . -12)) env))
;;   (emit "ret"))


;; (define (comp-f3z-def x si env)
;;   ;; E[x]
;;   (emit "f3z: nop")
;;   (comp 'z
;; 	si
;; 	(append '((x . -4)(y . -8)(z . -12)) env))
;;   (emit "ret"))


;; (define (comp-f3x x si env)
;;   ;; E[x]  si - 4
;;   (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
;;     ;; save onto stack
;;   (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ")
  
;;   ;; E[y]  si - 8 
;;   (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
;;   ;; save onto stack
;;   (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ")
  
;;   ;; E[z]  si - 12
;;   (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
;;   ;; save onto stack
;;   (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ")

;;   ;; adjust ESP , si is negative multiple of 4 :  -4 , -8 , -12 , -16, -20
;;   (emit "add dword esp , " (+ si 4))

;;   ;; do the CALL
;;   (emit "call f3x")

;;   ;; 
;;   (emit "add dword esp , " (- (+ si 4))))




;; (define (comp-f3y x si env)
;;   ;; E[x]  si - 4
;;   (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
;;     ;; save onto stack
;;   (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ")
  
;;   ;; E[y]  si - 8 
;;   (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
;;   ;; save onto stack
;;   (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ")
  
;;   ;; E[z]  si - 12
;;   (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
;;   ;; save onto stack
;;   (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ")

;;   ;; adjust ESP , si is negative multiple of 4 :  -4 , -8 , -12 , -16, -20
;;   (emit "add dword esp , " (+ si 4))

;;   ;; do the CALL
;;   (emit "call f3y")

;;   ;; 
;;   (emit "add dword esp , " (- (+ si 4))))


;; (define (comp-f3z x si env)
;;   ;; E[x]  si - 4
;;   (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
;;     ;; save onto stack
;;   (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ")
  
;;   ;; E[y]  si - 8 
;;   (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
;;   ;; save onto stack
;;   (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ")
  
;;   ;; E[z]  si - 12
;;   (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
;;   ;; save onto stack
;;   (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ")

;;   ;; adjust ESP , si is negative multiple of 4 :  -4 , -8 , -12 , -16, -20
;;   (emit "add dword esp , " (+ si 4))

;;   ;; do the CALL
;;   (emit "call f3z")

;;   ;; 
;;   (emit "add dword esp , " (- (+ si 4))))




;; (define (comp-fib-def x si env)
;;   ;; E[x]
;;   (emit "fib: nop")
;;   (comp '(if (= n 1)
;; 	     1
;; 	     (if (= n 2)
;; 		 1
;; 		 (+ (fib (- n 1))
;; 		    (fib (- n 2)))))
;; 	;;  1 arg = 2 wordsizes 
;;   	(- (* 2 *wordsize*))
;;   	(append '((n . -4)) env))
;;   (emit "ret"))







;; (define (comp-fac-def x si env)
;;   ;; E[x]
;;   (emit "fac: nop")
;;   (comp '(if (= n 1)
;; 	     1
;; 	     (* n (fac (- n 1))))
;; 	;;  1 arg = 2 wordsizes  huh.
;;   	(- (* 2 *wordsize*))
;;   	(append '((n . -4)) env))
;;   (emit "ret"))



;; (define (comp-tak-def x si env)
;;   ;; E[x]
;;   (emit "")
;;   (emit "tak1: nop")
;;   (emit "      jmp tak")
;;   (emit "")  
;;   (emit "tak2: nop")
;;   (emit "      jmp tak")
;;   (emit "")    
;;   (emit "tak3: nop")
;;   (emit "      jmp tak")
;;   (emit "")
;;   (emit "tak4: nop")
;;   (emit "      jmp tak")
;;   (emit "")
  
;;   (emit "tak: nop")
;;   (comp '(begin
;; 	   ;;(debug-tak)
;; 	   (if (< y x)
;; 	       (tak4 (tak1 (- x 1) y z)
;; 		     (tak2 (- y 1) z x)
;; 		     (tak3 (- z 1) x y))
;; 	       z))
;; 	;; 3 args = 4 wordsize huh.
;;   	(- (* 4 *wordsize*))
;;   	(append '((x . -4)(y . -8)(z . -12)) env))
;;   (emit "ret"))


;; (define (comp-tak x si env)
;;   ;; (TAK x y z)
;;   ;; si = keep empty for ESP to go into.
;;   ;;(emit "nop ; starts TAK here")
;;   ;; E[x]  si - 4
;;   (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
;;   (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ; save X ")
;;   ;; E[y]  si - 8 
;;   (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
;;   (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ; save Y")
;;   ;; E[z]  si - 12
;;   (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
;;   (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ; save Z" )
;;   (emit "add dword esp , " (+ si *wordsize*) "; adjust esp")
;;   (emit "call tak ; " (- si (* 3 *wordsize*)))
;;   (emit "sub dword esp , " (+ si *wordsize*) "; restore esp"))




;; compile arg one by one and push onto stack

;; (F x y z)
;; si = keep empty for ESP to go into.
;;(emit "nop ; starts TAK here")
;; E[x]  si - 4

;; (comp (car (cdr x)) (- si (* 1 *wordsize*)) env)
;; (emit "mov dword [ esp " (- si (* 1 *wordsize*)) "] , eax ; save X ")
;; ;; E[y]  si - 8 
;; (comp (car (cdr (cdr x))) (- si (* 2 *wordsize*)) env)
;; (emit "mov dword [ esp " (- si (* 2 *wordsize*)) "] , eax ; save Y")
;; ;; E[z]  si - 12
;; (comp (car (cdr (cdr (cdr x)))) (- si (* 3 *wordsize*)) env)
;; (emit "mov dword [ esp " (- si (* 3 *wordsize*)) "] , eax ; save Z" )

;; leave 2 slots free
;;
;; si is overwhelmingly negative
;;
;; si - 0  : return address goes here  * slot 1 *
;; si - 4  : closure ptr untagged      * slot 2 *
;; si - 8  : arg 1
;; si - 12 : arg 2
;; si - 16 : arg 3



;; before call ESP = si + 4
;;  when CALL opcode executed ESP decremented then 

;; si + 4   :  before DO the CALL
;; si       :  where return address will go 
;; si - 4   :  closure ptr
;; si - 8   :  arg1
;; si - 12  :  arg2
;; si - 16  :  arg3
;;

;; 4 argument procedure for example
;; e.g (f4 a b c d)
;; ???????????
;; arg4  d 
;; arg3  c
;; arg2  b 
;; arg1  a
;; ??????????
(define (comp-application-helper args si env)
  (cond
   ((null? args) #f)
   (else (begin
	   (comp (car args) si env)
	   ;;(emit "mov dword [esp " si "] , eax")
	   (emit "push dword eax") 
	   ;;(emit "push dword eax") ;; must NEVER !! do this -- destroys ESP frame pointer
	   ;;(emit "mov dword [ ebp - " si-offset "] , eax ; save arg " index)
	   (comp-application-helper (cdr args) (- si word) env)))))




;; stack to be like this
;;
;; [ empty slot for return ip to go when do CALL ]
;; closure ptr
;; arg1
;; arg2
;; arg3
;; arg4


(define (comp-application x si env)
  (let ((fn (car x))
	(args (cdr x)))

    (display "* regular function call *")
    (newline)
    (display "* FN * = ")
    (display fn)
    (newline)
    (display "* ARGS * = ")
    (display args)
    (newline)
        
    ;; compile arguments
    ;; reserve two slots for RETURN-ADDRESS and CLOSURE-PTR slot
    (comp-application-helper (reverse args) si env)
    
    ;; compile procedure
    (comp fn si env)
        
    ;; untag the closure in EAX
    ;; -8 is binary 11...1111000  lower 3 bits zero")
    (emit "and dword eax , -8 ; untag closure ")
    
    ;; save un- tagged - closure on stack
    ;;(emit "mov dword [esp "  (- si word) " ] , eax ; closure ptr ")
    (emit "push dword eax ; closure ptr")
    
    ;;(emit "push dword eax")
    ;;(emit "sub dword eax , 110b ; untag closure ")
    ;;(emit "mov dword [ebp - " (+ si 4) " ] , eax ; closure ptr ")   
        
    ;;(comp fn (- si (* (+ 2 (length args)) word)) env)
    ;; presumably there is a closure left in EAX register
    ;;(emit "mov dword eax , [esp " (- si word) "] ; closure ptr ")   
    ;; save closure ptr
    ;;(emit "mov dword [esp - 4 ] , eax ; closure ptr ")
    ;; get closure into eax
    ;;(emit "mov dword eax , [esp " (- si word) "]  ; untag closure ")
    ;; save closure onto stack
    ;;(emit "mov dword [esp " (- si word) "] ,eax  ; raw closure ptr ")
    ;; obtain procedure CODE address
    (emit "mov dword eax , [eax] ; load CODE address ")
    
    ;;(let ((adjust (+ si word)))
    
    ;;(emit "add dword esp , " (+ si word) "; adjust stack")
    (emit "call eax ; call closure")

    (emit "add dword esp , " (* word (+ 1 (length args))))
    
    ;;(emit "sub dword esp , " (+ si word) "; restore esp")
    ))








(define (esp n)
  (if (< n 0)
      (begin
	(number->string n))
      (begin
	(string-append "+" (number->string n)))))



(define (comp-tailcall-collapse args si index env)
  (cond
   ((null? args) #f)
   (else (begin
	   (let ((offset (* index word)))
	     (emit "mov dword eax , [ esp " (- si offset) "] ; collapse element " index)
	     (emit "mov dword [ esp - " offset "] , eax ")
	     (comp-tailcall-collapse (cdr args) si (+ index 1) env))))))



;; x = (tailcall (fn . args))
(define (comp-tailcall-application x si env)
  (let ((fn (car (car (cdr x))))
	(args (cdr (car (cdr x)))))
    
    (display "* tail call optimisation *") (newline)
    (display "* FN * = ")
    (display fn)
    (newline)
    (display "* ARGS * = ")
    (display args)
    (newline)

    ;; ------- first section is verbatim of normal procedure application ----------
    ;; compile arguments
    ;; reserve two slots for RETURN-ADDRESS and CLOSURE-PTR slot
    (comp-application-helper args (- si (* 2 word)) env)

    ;; compile procedure
    (comp fn (- si (* (+ 2 (length args)) word)) env)
    
    ;; untag the closure in EAX 
    (emit "and dword eax , -8 ; untag closure -8 is binary 11...1111000  lower 3 bits zero")

    ;;(emit "mov dword [esp "  (- si word) " ] , eax ; closure ptr ")
    ;; save un- tagged - closure on stack directly 
    (emit "mov dword [esp "  (- word) " ] , eax ; closure ptr ")

    ;; obtain procedure CODE address
    ;; placed it into EBX register because tailcall-collapse over writes EAX register
    (emit "mov dword ebx , [eax] ; load CODE address ")
    
    ;; collapse the stack
    ;;   leave original return ip intact at ESP [ 0 ]
    ;;   put procedure closure at ESP [ - 4 ]
    ;;   put arg1 at ESP [ - 8 ]
    ;;   put arg2 at ESP [ - 12 ]
    ;;   put arg3 at ESP [ - 16 ]
    (comp-tailcall-collapse args si 2 env)

    ;; CODE address is still safe in EBX register , so just jump to it
    ;; now do the tail call
    (emit "jmp ebx ; tail call")))







    
    ;; ;; compile arguments
    
    ;; (comp-application-helper args si 2 env)
    
    ;; ;; evaluate operator
    ;; (comp fn (- si (* (+ 2 (length args)) word)) env)

    ;; ;; presumably there is a closure left in EAX register
    ;; ;; untag it
    ;; ;;(emit "sub dword eax , 110b ; untag closure ")
    
    ;; ;; save closure onto stack
    ;; (emit "mov dword [esp " (- si word) "] ,eax  ; raw closure ptr ")
    
    ;; ;; collapse arguments
    ;; (emit "nop ; collapse arguments by " si)
    
    ;; ;;(comp-tailcall-collapse (cons 'dummy args) si 2 env)
    ;; ;;(comp-tailcall-collapse (cons 'dummy (cons 'dummy2 args)) si 1 env)
    

    ;; (emit "mov dword eax , [esp " (- si word) "] ; closure ptr ")
    ;; (emit "mov dword [esp " (- word) "] , eax ; closure ptr ")
    
    ;; ;; save closure ptr
    ;; ;;(emit "mov dword [esp - 4 ] , eax ; closure ptr ")
    
    ;; ;; untag the closure
    ;; (emit "sub dword eax , 110b ; untag closure ")
    
    ;; ;; obtain procedure CODE address
    ;; (emit "mov dword eax , [eax] ; load procedure ptr from raw closure ")
    
    ;; ;; now the tail call
    ;; (emit "jmp eax ;  tailcall")))


















;; (define (comp-tak-tail-recursive x si env)
;;   ;; (TAK x y z)
;;   ;; si = keep empty for ESP to go into.
;;   ;;(emit "nop ; starts TAK here")
;;   ;; E[x]  si - 4
;;   (comp (car (cdr x)) (- si (* 1 word)) env)
;;   (emit "mov dword [ esp " (- si (* 1 word)) "] , eax ; save X ")
;;   ;; E[y]  si - 8 
;;   (comp (car (cdr (cdr x))) (- si (* 2 word)) env)
;;   (emit "mov dword [ esp " (- si (* 2 word)) "] , eax ; save Y")
;;   ;; E[z]  si - 12
;;   (comp (car (cdr (cdr (cdr x)))) (- si (* 3 word)) env)
;;   (emit "mov dword [ esp " (- si (* 3 word)) "] , eax ; save Z" )
  
;;   (emit "nop ; collapse arguments by " si)
  
;;   (emit "mov dword eax , [esp  " (- si (* 1 word)) "] ; collapse X ")
;;   (emit "mov dword [esp  " (- (* 1 word)) "] , eax ")

;;   (emit "mov dword eax , [esp  " (- si (* 2 word)) "] ; collapse Y")
;;   (emit "mov dword [esp  " (- (* 2 word)) "] , eax ")

;;   (emit "mov dword eax , [esp  " (- si (* 3 word)) "] ; collapse Z")
;;   (emit "mov dword [esp  " (- (* 3 word)) "] , eax ")
  
;;   ;;(emit "add dword esp , " (+ si word) "; adjust esp")
;;   (emit "jmp tak4 ; allow for gdb to record ARGS to TAK")
;;   ;;(emit "sub dword esp , " (+ si word) "; restore esp")
;;   )





;; (define (comp-tak-generic nth-tak x si env)
;;   (cond
;;    ((= nth-tak 4) ;; TAK4 is tail recursive
;;     (comp-tak-tail-recursive x si env))
;;    ;;(#f #f)
;;    (else
;;     (begin
;;       ;; (TAK x y z)
;;       ;; si = keep empty for ESP to go into.
;;       ;;(emit "nop ; starts TAK here")
;;       ;; E[x]  si - 4
;;       (comp (car (cdr x)) (- si (* 1 word)) env)
;;       (emit "mov dword [ esp " (- si (* 1 word)) "] , eax ; save X ")
;;       ;; E[y]  si - 8 
;;       (comp (car (cdr (cdr x))) (- si (* 2 word)) env)
;;       (emit "mov dword [ esp " (- si (* 2 word)) "] , eax ; save Y")
;;       ;; E[z]  si - 12
;;       (comp (car (cdr (cdr (cdr x)))) (- si (* 3 word)) env)
;;       (emit "mov dword [ esp " (- si (* 3 word)) "] , eax ; save Z" )
;;       (emit "add dword esp , " (+ si word) "; adjust esp")
;;       (emit "call tak" nth-tak " ; " (- si (* 3 word)))
;;       (emit "sub dword esp , " (+ si word) "; restore esp")))))


;; ;; this debug code is not used
;; (define (comp-debug-tak x si env)
;;   (emit "pushad")
;;   (emit "push dword esp")
;;   (emit "call debug_stack")
;;   (emit "add dword esp , 4")
;;   (emit "popad"))











;; (define (comp-fib x si env)
;;   ;; (FIB n)

;;   ;; si = keep empty for ESP to go into.
;;   ;;(emit "nop ; FIB call now ")
  
;;   ;; E[x]  si - 4
;;   (comp (car (cdr x)) (- si (* 1 word)) env)
  
;;     ;; save onto stack
;;   (emit "mov dword [ esp " (- si (* 1 word)) "] , eax ")
  
;;   ;; ;; E[y]  si - 8 
;;   ;; (comp (car (cdr (cdr x))) (- si (* 2 word)) env)
;;   ;; ;; save onto stack
;;   ;; (emit "mov dword [ esp " (- si (* 2 word)) "] , eax ")
  
;;   ;; ;; E[z]  si - 12
;;   ;; (comp (car (cdr (cdr (cdr x)))) (- si (* 3 word)) env)
;;   ;; ;; save onto stack
;;   ;; (emit "mov dword [ esp " (- si (* 3 word)) "] , eax ")

;;   ;; adjust ESP , si is negative multiple of 4 :  -4 , -8 , -12 , -16, -20
;;   (emit "add dword esp , " (+ si word))

;;   ;; do the CALL
;;   (emit "call fib")
;;   ;; 
;;   (emit "sub dword esp , " (+ si word)))

;; ;;(- (+ si 4))))









;; (define (comp-fac x si env)
;;   ;; (FAC n)

;;   ;; si = keep empty for ESP to go into.
;;   ;;(emit "nop ; FIB call now ")
  
;;   ;; E[x]  si - 4
;;   (comp (car (cdr x)) (- si (* 1 word)) env)
  
;;     ;; save onto stack
;;   (emit "mov dword [ esp " (- si (* 1 word)) "] , eax ")
  
;;   ;; ;; E[y]  si - 8 
;;   ;; (comp (car (cdr (cdr x))) (- si (* 2 word)) env)
;;   ;; ;; save onto stack
;;   ;; (emit "mov dword [ esp " (- si (* 2 word)) "] , eax ")
  
;;   ;; ;; E[z]  si - 12
;;   ;; (comp (car (cdr (cdr (cdr x)))) (- si (* 3 word)) env)
;;   ;; ;; save onto stack
;;   ;; (emit "mov dword [ esp " (- si (* 3 word)) "] , eax ")

;;   ;; adjust ESP , si is negative multiple of 4 :  -4 , -8 , -12 , -16, -20
;;   (emit "add dword esp , " (+ si word))

;;   ;; do the CALL
;;   (emit "call fac")
;;   ;; 
;;   (emit "sub dword esp , " (+ si word)))












    

    ;; (if (and (pair? val)
    ;; 	     (eq? (car val) 'lambda))
    ;; 	(comp-define-lambda x si env)
    ;; 	#f)))
    

;; assume all toplevel defines are procedures
;; generate a jump label
;; jump over definition as theres nothing to do here...
;; definitions dont do anything as such ...
;;
;; we could generate closure and so forth ,
;; but at toplevel no need since everything already in scope
;;
;; (lambda (x y z)
;;  x = -4 
;;  y = -8
;;  z = -12
;;   on entry

;; (define (comp-define-helper args index)
;;   (cond
;;    ((null? args) '())
;;    (else (let ((sym (car args)))
;; 	   (cons (list sym 'local index)
;; 		 (comp-define-helper (cdr args) (- index word)))))))


;; (define (comp-define-lambda x si env)
;;   (let ((name (car (cdr x)))
;; 	(proc (car (cdr (cdr x))))
;; 	(after-label (gensym "after")))
;;     (let ((args (car (cdr proc)))
;; 	  (body (cdr (cdr proc))))
;;       (let ((n-args (length args)))
;; 	(let ((extra-env (comp-define-helper args (- word))))
	  
;; 	  (display "* comp-define-lambda * : ")
;; 	  (display "extra env = ")
;; 	  (display extra-env)
;; 	  (newline)
	
;; 	(emit "jmp " after-label)    
;; 	(emit (tidy-proc-name name) ": nop")
;; 	;; compile the definition here
;; 	(comp `(begin ,@body)
;; 	      (- (* (+ n-args 1) word))
;; 	      (append extra-env env))
;; 	;; final return 
;; 	(emit "ret")
;; 	;; jump over definition
;; 	(emit after-label ": nop"))))))



(define (comp-lambda-helper args)
  (define (helper args index)
    (cond
     ((null? args) '())
     (else (let ((sym (car args)))
	     (cons (list sym 'local index)
		   (helper (cdr args) (+ index word)))))))
  (helper args 8))




;; suppose free variables are a b c d
;; assigns closure offsets of multiples of 8 byte boundary.
;; a : 8
;; b : 16
;; c : 24
;; d : 32
;;

;; arguments are placed in closure at offset word
;; then every word thereafter ... word*2 word*3 word*4 ...
(define (comp-closure-helper args)
  (define (helper args index)
    (cond
     ((null? args) '())
     (else (let ((sym (car args)))
	     (cons (list sym 'closure index)
		   (helper (cdr args) (+ index word)))))))
  (helper args word))


(define (remove-primitives vars)
  (cond
   ((null? vars) '())
   (else (let ((var (car vars)))
	   (if (member var *primitives*)
	       (remove-primitives (cdr vars))
	       (cons var (remove-primitives (cdr vars))))))))


(define (remove-toplevel vars env)
  (cond
   ((null? vars) '())
   (else (let ((var (car vars)))
	   (if (and (assoc var env)
		    (toplevel-binding? (assoc var env)))
	       (begin ;; ignore this as its toplevel bound	       
		 (remove-toplevel (cdr vars) env))
	       (begin ;; include it - not toplevel bound
		 (cons var (remove-toplevel (cdr vars) env))))))))




;; for each formal parameter [ <args> ]  of the lambda expression
;; (lambda <args> body)
(define (comp-lambda x si env)
  (let ((anon-name (gensym "lambda"))	
	(args (car (cdr x)))
	(body (cdr (cdr x)))
	(after-label (gensym "after"))
	(free-variables (remove-toplevel (remove-primitives (freevars x)) env)))
      (let ((n-args (length args)))
	(let ((extra-env (comp-lambda-helper args))
	      (free-env  (comp-closure-helper free-variables))) 
	  (let ((new-env (append extra-env free-env env )))
	      
	  (display "* comp-lambda * : ")
	  (display "extra env = ")
	  (display extra-env)
	  (newline)

	  (display "* comp-lambda * : ")
	  (display "free = ")
	  (display free-variables)
	  (newline)
	  (display "free-env = ")
	  (display free-env)
	  (newline)

	  (display "* comp-lambda * : ")
	  (display "new-env = ")
	  (display new-env)
	  (newline)
	  
	  
	  (emit "jmp " after-label)
	  ;; cannot just do arbitrary PUSH and POPS
	  ;; lambda has no name	  
	  (emit anon-name ": nop ; ")

	  (emit "push dword ebp ; entry prologue")
	  (emit "mov dword ebp , esp")

	  
	  
	  ;; compile the definition here with extra formals and free variables
	  ;; new stack index number args and 2 reserved return ip and closure ptr
	  ;; e.g n-args = 0 then new-si of -8
	  ;; return ip   : esp + 0
	  ;; closure ptr : esp - 4
	  ;; empty slot  : esp - 8 , 
	  (let ((new-si (- (* (+ n-args 2) word))))	    
	    (comp `(begin ,@body) new-si new-env))


	  (emit "mov dword esp , ebp ; exit prolog")
	  (emit "pop dword ebp")	  
	  (emit "ret")
	  
	  ;; jump over definition
	  (emit after-label ": nop")

	  

	  ;; ??
	  ;;(emit "push dword esi")
	  
	  ;; construct closure
	  ;; remember where HEAP esi was before build closure
	  (emit "mov dword ebx , esi")
	  
	  ;; 1st CODE ptr to anon-name
	  ;; closure ptr	  
	  (emit "mov dword [ esi ] , " anon-name)
	  	  
	  ;; bump heap 
	  (emit "add dword esi , 4 ")
	  
	  ;; for each free variable ,
	  ;; 1st .. closure arg 0 ... + 4 
	  ;; 2nd .. closure arg 1 ... + 8
	  ;; 3rd .. closure arg 2 ... + 12
	  ;; 4th .. closure arg 3 ... + 16
	  (map (lambda (f)
		 (begin
		   ;;(comp-lookup f (+ si word) env)
		   ;; should be free to re-use si slot for each arg
		   (comp-lookup f si env)
		   ;; 
		   (emit "mov dword [ esi ] , eax")
		   ;; bump esi
		   (emit "add dword esi , 4 ")
		   ))
	       free-variables)

	  ;; important HEAP pointer esi is a multiple of 8
	  (emit-align-heap-pointer)
	  
	  (emit "mov dword eax , ebx")
	  
	  ;; tag as closure
	  (emit "or dword eax , 110b ")
	  ;;(emit "add dword eax , 110b ")
	  )))))






(define (comp x si env)
  (cond
   ((symbol? x) (comp-lookup x si env))
   ((null? x) (comp-null x si env))
   ((boolean? x)  (comp-boolean x si env))
   ((char? x) (comp-char x si env))    
   ((integer? x) (comp-integer x si env))
   ;; quoted '()  empty list is just the empty list
   ((and (pair? x)
	 (eq? (car x) 'quote)
	 (null? (car (cdr x))))
    (comp-null x si env))
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

   ;; bindings
   ((and (pair? x) (eq? (car x) 'let)) (comp-let x si env))
   
   ;; explicit lambda
   ((and (pair? x) (eq? (car x) 'lambda)) (comp-lambda x si env))

   
   ;; explicit tailcall
   ;;((and (pair? x) (eq? (car x) 'tailcall)) (comp-tailcall-application x si env))
   
   ;; tak 
   ;;((and (pair? x) (eq? (car x) 'tak)) (comp-tak x si env))
   
   ;;((and (pair? x) (eq? (car x) 'tak1)) (comp-tak-generic 1 x si env))
   ;;((and (pair? x) (eq? (car x) 'tak2)) (comp-tak-generic 2 x si env))
   ;;((and (pair? x) (eq? (car x) 'tak3)) (comp-tak-generic 3 x si env))
   ;;((and (pair? x) (eq? (car x) 'tak4)) (comp-tak-generic 4 x si env))

   ;; f3[xyz] x y z
   ;;((and (pair? x) (eq? (car x) 'f3x)) (comp-f3x x si env))
   ;;((and (pair? x) (eq? (car x) 'f3y)) (comp-f3y x si env))
   ;;((and (pair? x) (eq? (car x) 'f3z)) (comp-f3z x si env))

   ;; fib
   ;;((and (pair? x) (eq? (car x) 'fib)) (comp-fib x si env))

   ;; factorial fac
   ;;((and (pair? x) (eq? (car x) 'fac)) (comp-fac x si env))

   ;; debug-tak
   ;;((and (pair? x) (eq? (car x) 'debug-tak)) (comp-debug-tak x si env))

   ((pair? x) (comp-application x si env))
   
   (else
    (error "comp : unknown expression : " x))))






(define (set-emit-output-port! p)
  (set! *out* p))




;; compilation is driven from another file [ lisp.scm ]

;; (define (compile-program input output)
;;   (call-with-input-file input
;;     (lambda (in-port)
;;       (let ((expr (read in-port)))
;; 	(call-with-output-file output
;; 	  (lambda (port)
;; 	    (set-emit-output-port! port)
	    
;; 	    (emit "")
;; 	    (emit "extern debug_stack")
;; 	    (emit "global scheme_entry")
	    
;; 	    ;; here compile the expression
;; 	    ;; initial stack index is negative wordsize
;; 	    ;; as [ esp - 4 ] , since esp holds return address.
;; 	    (let ((initial-environment '())
;; 		  (stack-index (- word)))

;; 	      ;;(comp-tak-def #f stack-index initial-environment)

;; 	      ;; (comp-fib-def #f stack-index initial-environment)
;; 	      ;; (comp-fac-def #f stack-index initial-environment)
;; 	      ;; (comp-f3x-def #f stack-index initial-environment)
;; 	      ;; (comp-f3y-def #f stack-index initial-environment)
;; 	      ;; (comp-f3z-def #f stack-index initial-environment)
	      
;; 	      (emit "scheme_entry: nop ")	      
;; 	      ;; HEAP is passed as 1st argument
;; 	      (emit "mov dword esi , [ esp + 4 ] ")
;; 	      (emit "scheme_heap_in_esi: nop")
	      
;; 	      (comp expr stack-index initial-environment)	      
;; 	      (emit "ret"))
	    
;; 	    (emit "")
;; 	    (emit "")
;; 	    (emit "")))))))











	    























