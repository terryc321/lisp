


;;; translate s expression to nasm

(define (gen-error x port)
  (error "CODE :GEN: unknown expr " x " port:" port))




;; just do MAP TRANSLATE code-list
(define (gen-dispatch x port)
  (cond
   ((and (pair? x) (eq? (car x) 'global))  (gen-global x port))
   ((and (pair? x) (eq? (car x) 'section))  (gen-section x port))
   ((and (pair? x) (eq? (car x) 'align))  (gen-align x port))
   ((and (pair? x) (eq? (car x) 'literal))  (gen-literal x port))
   ((and (pair? x) (eq? (car x) 'label))  (gen-label x port))
   ((and (pair? x) (eq? (car x) 'ret))  (gen-return x port))
   ((and (pair? x) (eq? (car x) 'mov))     (gen-mov x port))
   ((and (pair? x) (eq? (car x) 'jmp))     (gen-jmp x port))
   ((and (pair? x) (eq? (car x) 'je))     (gen-je x port))
   ((and (pair? x) (eq? (car x) 'push))     (gen-push x port))
   ((and (pair? x) (eq? (car x) 'pop))     (gen-pop x port))  
   ((and (pair? x) (eq? (car x) 'add))     (gen-add x port))  
   ((and (pair? x) (eq? (car x) 'sub))     (gen-sub x port))  
   ((and (pair? x) (eq? (car x) 'div))     (gen-div x port))  
   ((and (pair? x) (eq? (car x) 'mul))     (gen-mul x port))  
   ((and (pair? x) (eq? (car x) 'or))     (gen-or x port))  
   ((and (pair? x) (eq? (car x) 'and))     (gen-and x port))  
   ((and (pair? x) (eq? (car x) 'cmp))     (gen-cmp x port))  
   ((and (pair? x) (eq? (car x) 'call))     (gen-call x port))  
   ((and (pair? x) (eq? (car x) 'comment))     (gen-comment x port))  
   ((and (pair? x) (eq? (car x) 'shl))     (gen-shl x port))  
   ((and (pair? x) (eq? (car x) 'shr))     (gen-shr x port))  
   ((and (pair? x) (eq? (car x) 'inc))     (gen-increment x port))  
   ((and (pair? x) (eq? (car x) 'dec))     (gen-decrement x port))  

   (else
    (gen-error x port))))

(define (gen x port)
  (display "generating code for ")
  (display x)
  (newline)
  (gen-dispatch x port)
  (newline port)
  ;;(display "... done\n")
  )








(define (gen-comment x port)
  (display ";; " port)
  (map (lambda (message)
	 (display message port)
	 (display " " port))
       (cdr x)))



(define (gen-global x port)
  (display "global " port)
  (display (car (cdr x)) port))


(define (gen-section x port)
  (cond
   ((eq? (car (cdr x)) 'data)
    (display "section .data" port))
   ((eq? (car (cdr x)) 'text)
    (display "section .text" port))
   (else (gen-error x port))))

(define (gen-align x port)
  (cond
   ((integer? (car (cdr x)))
    (display "align " port)
    (display (car (cdr x)) port))
   (else (gen-error x port))))


(define (gen-literal x port)
  (define (helper xs)
    (cond
     ((null? xs) #f)
     (else (display (car xs) port)
	   (helper (cdr xs)))))
  (helper (cdr x)))


(define (gen-label x port)
  (display (car (cdr x)) port)
  (display ": nop" port))







(define (gen-return x port)
  (display "ret" port))

(define (gen-mov x port)
  (begin
    (display "MOV DWORD " port)
    (gen-rm (car (cdr x)) port)
    (display " , " port)
    (gen-rmi (car (cdr (cdr x))) port)))

(define (gen-mul x port)
  (begin
    (display "MUL DWORD " port)
    (gen-rmi (car (cdr x)) port)))

(define (gen-div x port)
  (begin
    (display "DIV DWORD " port)
    (gen-rmi (car (cdr x)) port)))





(define (gen-add x port)
  (begin
    (display "ADD DWORD " port)
    (gen-rm (car (cdr x)) port)
    (display " , " port)
    (gen-rmi (car (cdr (cdr x))) port)))

(define (gen-sub x port)
  (begin
    (display "SUB DWORD " port)
    (gen-rm (car (cdr x)) port)
    (display " , " port)
    (gen-rmi (car (cdr (cdr x))) port)))

(define (gen-or x port)
  (begin
    (display "OR DWORD " port)
    (gen-rm (car (cdr x)) port)
    (display " , " port)
    (gen-rmi (car (cdr (cdr x))) port)))


(define (gen-increment x port)
  (begin
    (display "INC DWORD " port)
    (gen-rm (car (cdr x)) port)))

(define (gen-decrement x port)
  (begin
    (display "DEC DWORD " port)
    (gen-rm (car (cdr x)) port)))



(define (gen-and x port)
  (begin
    (display "AND DWORD " port)
    (gen-rm (car (cdr x)) port)
    (display " , " port)
    (gen-rmi (car (cdr (cdr x))) port)))

(define (gen-cmp x port)
  (begin
    (display "CMP DWORD " port)
    (gen-rm (car (cdr x)) port)
    (display " , " port)
    (gen-rmi (car (cdr (cdr x))) port)))


(define (gen-shl x port)
  (begin
    (display "SHL DWORD " port)
    (gen-rm (car (cdr x)) port)
    (display " , " port)
    (gen-rmi (car (cdr (cdr x))) port)))

(define (gen-shr x port)
  (begin
    (display "SHR DWORD " port)
    (gen-rm (car (cdr x)) port)
    (display " , " port)
    (gen-rmi (car (cdr (cdr x))) port)))


(define (gen-call x port)
  (cond
   ((gen-label? (car (cdr x)))
    (begin
      (display "CALL " port)
      (display (car (cdr (car (cdr x)))) port)))
   ((gen-register? (car (cdr x)))
    (begin
      (display "CALL " port)
      (gen-rm (car (cdr x)) port)))
   (else (gen-error x port))))


(define (gen-je x port)
  (cond
   ((gen-label? (car (cdr x)))
    (begin
      (display "JE " port)
      (display (car (cdr (car (cdr x)))) port)))
   ((gen-register? (car (cdr x)))
    (begin
      (display "JE " port)
      (gen-rm (car (cdr x)) port)))
   (else (gen-error x port))))



(define (gen-jmp x port)
  (cond
   ((gen-label? (car (cdr x)))
    (begin
      (display "JMP " port)
      (display (car (cdr (car (cdr x)))) port)))
   ((gen-register? (car (cdr x)))
    (begin
      (display "JMP " port)
      (gen-rm (car (cdr x)) port)))
   (else (gen-error x port))))



(define (gen-push x port)
  (cond
   ((gen-register? (car (cdr x)))
    (begin
      (display "PUSH " port)
      (gen-rm (car (cdr x)) port)))
   (else (gen-error x port))))


(define (gen-pop x port)
  (cond
   ((gen-register? (car (cdr x)))
    (begin
      (display "POP " port)
      (gen-rm (car (cdr x)) port)))
   (else (gen-error x port))))




;; (define (gen-add x port)
;;   #t)

;; (define (gen-sub x port)
;;   #t)

;; (define (gen-mul x port)
;;   #t)

;; (define (gen-div x port)
;;   #t)



(define (gen-register? x)
  (memq x '(eax ebx ecx edx edi esi esp ebp
		EAX EBX ECX EDX EDI ESI ESP EBP)))

(define (gen-label? x)
  (and (pair? x)
       (eq? (car x) 'label)))



(define (gen-ref x port)
    (display "[" port)
    (let ((the-ref (car (cdr x))))
      (cond
       ((symbol? the-ref) (gen-rm the-ref port))
       
       ((and (pair? the-ref)
	     (eq? (car the-ref) '+)
	     (gen-register? (car (cdr the-ref)))
	     (integer? (car (cdr (cdr the-ref)))))
	(let ((the-int (car (cdr (cdr the-ref)))))
	  (cond
	   ((< the-int 0)
	    (gen-rmi (car (cdr the-ref)) port)
	    (display " - " port)
	    (display (abs the-int) port))
	   ((= the-int 0)
	    (gen-rmi (car (cdr the-ref)) port))
	   ((> the-int 0)
	    (gen-rmi (car (cdr the-ref)) port)
	    (display " + " port)
	    (display the-int port)))))
       
       ((and (pair? the-ref)
	     (eq? (car the-ref) '-)
	     (gen-register? (car (cdr the-ref)))
	     (integer? (car (cdr (cdr the-ref)))))
	(let ((the-int (car (cdr (cdr the-ref)))))
	  (cond
	   ((< the-int 0)
	    (gen-rmi (car (cdr the-ref)) port)
	    (display " + " port)
	    (display (abs the-int) port))
	   ((= the-int 0)
	    (gen-rmi (car (cdr the-ref)) port))
	   ((> the-int 0)
	    (gen-rmi (car (cdr the-ref)) port)
	    (display " - " port)
	    (display the-int port)))))
       (else (gen-error the-ref port))))
    (display "]" port))








   


(define (gen-rm x port)
  (cond
   ((string? x) (display x port))
   ((eq? x 'eax) (display " EAX " port))
   ((eq? x 'ebx) (display " EBX " port))
   ((eq? x 'ecx) (display " ECX " port))
   ((eq? x 'edx) (display " EDX " port))
   ((eq? x 'esp) (display " ESP " port))
   ((eq? x 'esi) (display " ESI " port))
   ((eq? x 'edi) (display " EDI " port))
   ((eq? x 'ebp) (display " EBP " port))   
   ((and (pair? x)
	 (eq? (car x) 'ref))
    (gen-ref x port))
   (else
    (gen-error x port))))






(define (gen-rmi x port)
  (cond
   ((string? x) (display x port))
   ((integer? x) (display x port))
   ((eq? x 'eax) (display " EAX " port))
   ((eq? x 'ebx) (display " EBX " port))
   ((eq? x 'ecx) (display " ECX " port))
   ((eq? x 'edx) (display " EDX " port))
   ((eq? x 'esp) (display " ESP " port))
   ((eq? x 'esi) (display " ESI " port))
   ((eq? x 'edi) (display " EDI " port))
   ((eq? x 'ebp) (display " EBP " port))   
   ((integer? x) (display x port))
   ((and (pair? x)
	 (eq? (car x) 'ref))
    (gen-ref x port))
   ((and (pair? x)
	 (eq? (car x) 'label))
    (display (car (cdr x)) port))
   (else
    (gen-error x port))))










   
   






     
