

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
  
   (else
    (gen-error x port))))

(define (gen x port)
  (display "generating code for ")
  (display x)
  (newline)
  (gen-dispatch x port)
  (newline port)
  (display "... done\n"))






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
  (display ": nop\n" port))


(define (gen-return x port)
  (display "ret" port))

(define (gen-mov x port)
  (begin
    (display "MOV DWORD " port)
    (gen-rm (car (cdr x)) port)
    (display " , " port)
    (gen-rmi (car (cdr (cdr x))) port)))


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
   ((and (pair? x) (eq? (car x) 'ref))
    (display "[" port)
    (let ((the-ref (car (cdr x))))
      (cond
       ((symbol? the-ref) (gen-rm x port))
       ((and (pair? the-ref)
	     (eq? (car the-ref) '+)
	     (gen-register? (car (cdr the-ref)))
	     (integer? (car (cdr (cdr the-ref)))))
	(gen-rmi (car (cdr the-ref)) port)
	(display " + " port)
	(gen-rmi (car (cdr (cdr the-ref))) port))
       ((and (pair? the-ref)
	     (eq? (car the-ref) '-)
	     (gen-register? (car (cdr the-ref)))
	     (integer? (car (cdr (cdr the-ref)))))
	(gen-rmi (car (cdr the-ref)) port)
	(display " - " port)
	(gen-rmi (car (cdr (cdr the-ref))) port))
       (else (gen-error the-ref port))))
    (display "]" port))
   (else
    (gen-error x port))))




(define (gen-rmi x port)
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
   ((integer? x) (display x port))
   ((and (pair? x) (eq? (car x) 'ref))
    (display "[" port)
    (let ((the-ref (car (cdr x))))
      (cond
       ((symbol? the-ref) (gen-rm x port))
       ((and (pair? the-ref)
	     (eq? (car the-ref) '+)
	     (gen-register? (car (cdr the-ref)))
	     (integer? (car (cdr (cdr the-ref)))))
	(begin
	  (gen-rmi (car (cdr the-ref)) port)
	  (display " + " port)
	  (gen-rmi (car (cdr (cdr the-ref))) port)))
       ((and (pair? the-ref)
	     (eq? (car the-ref) '-)
	     (gen-register? (car (cdr the-ref)))
	     (integer? (car (cdr (cdr the-ref)))))
	(begin
	  (gen-rmi (car (cdr the-ref)) port)
	  (display " - " port)
	  (gen-rmi (car (cdr (cdr the-ref))) port))       )
       (else (gen-error the-ref port))))
    (display "]" port))
   (else
    (gen-error x port))))





   
   






     
