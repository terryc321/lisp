

;; Libraries 

;;; mit specific stuff
;;(define (gensym args) (cons 'gensym args)) ;;generate-uninterned-symbol)
;;(define gensym generate-uninterned-symbol)

;;quasiquotation
(load  	  "/home/terry/lisp/quasiquote/quasiquote.scm")

;; non hygienic macro expander
(load 	  "/home/terry/lisp/macro-expander/macro-expander.scm")

;; quasiquote expander
(load 	  "/home/terry/lisp/macros/quasiquote.scm")

;; cond is nested ifs
(load 	  "/home/terry/lisp/macros/cond.scm")

;; let is just applied lambda, but visual appearances easier keep as let
;;(load 	  "/home/terry/lisp/macros/let.scm")

;; letrec requires lets and sets
(load 	  "/home/terry/lisp/macros/letrec.scm")

;; let* are sequential lets
(load 	  "/home/terry/lisp/macros/let-star.scm")

;; when is just conditional with sequencing
(load 	  "/home/terry/lisp/macros/when.scm")

;; dont know if we need lambda expander , 
(load 	  "/home/terry/lisp/macros/lambda.scm")

;; conjunction
(load 	  "/home/terry/lisp/macros/and.scm")

;; disjunction
(load 	  "/home/terry/lisp/macros/or.scm")

;; define
(load 	  "/home/terry/lisp/macros/define.scm")

;; begin simplifier
(load 	  "/home/terry/lisp/macros/begin.scm")


;; alpha conversion
(load  	  "/home/terry/lisp/alpha-conversion/alpha.scm")

;; free variable analysis
(load  	  "/home/terry/lisp/free-variables/freevar.scm")

;; the rudimentary compiler
(load  	  "comp.scm")



;; read all the forms from a target expression
;; p is PORT
(define (read-file filename)
  (let ((forms 
	 (call-with-input-file filename
	   (lambda (p)
	     (let f ((x (read p)))
	       (if (eof-object? x)
		   '()
		   (cons x (f (read p)))))))))
    forms))



;; (define f x) definition
;; (f x y z) ;; application
;; (if x y z) ;; conditional
;; (let ((x ..)(y ...)(z ...)) ...)
;; (set! x y) ;; not implemented assignment this yet.


(define (stage-1 forms)
  (let ((me (macro-expand forms)))
    (newline)
    (display "stage-1: macro-expanded:")
    (newline)
    (display me)
    (newline)
    me))




    ;; (let ((ac (alpha-convert me)))
    ;;   (newline)
    ;;   (display "stage-1: alpha-converted:")
    ;;   (newline)
    ;;   (display ac)
    ;;   (newline)
    ;;   ac)))


      
;; if no toplevel , no slots used , then 4 result
;; if one slot used then 8
;; two slots 12
;; and so on...
;; or we could just say : (* word (+ 1 (length slots)))
(define (next-toplevel-stack-index-available slots)
  (define (search slots index)
    (cond
     ((null? slots) (+ index word))
     (else (search (cdr slots) (binding-stack-index (car slots))))))
  ;; if no toplevel , then default
  (search slots 0))




   



(define (toplevel-non-definitions forms)
  (define (toplevel-helper forms non-defs)
    (cond
     ((null? forms) (reverse non-defs))
     ((and (pair? (car forms))
	   (eq? 'define (car (car forms)))
	   (symbol? (car (cdr (car forms)))))
      ;; its a toplevel definition
      (toplevel-helper (cdr forms) non-defs))
     (else
      (toplevel-helper (cdr forms) (cons (car forms) non-defs)))))
  (toplevel-helper forms '()))



(define (toplevel-definitions forms)
  (define (toplevel-helper forms defs)
    (cond
     ((null? forms) (reverse defs))
     ((and (pair? forms)
	   (pair? (car forms))
	   (eq? 'define (car (car forms)))
	   (symbol? (car (cdr (car forms)))))
      ;; its a toplevel definition
      (toplevel-helper (cdr forms) (cons (car forms) defs)))
     (else
      (toplevel-helper (cdr forms) defs))))
  (toplevel-helper forms '()))






(define (assign-toplevel-stack-index syms index)
  (cond
   ((null? syms) '())
   (else (let ((sym (car syms)))
	   (cons (list sym 'toplevel index)
		 (assign-toplevel-stack-index (cdr syms) (+ index word)))))))



;; maybe try using EBP as base pointer to global environment , some initial stack space
;; which is reserved for globals.

;; only interested in (define f x)
;; toplevel environmnet will contain these f's
;; assign a unique stack index for these also
(define (toplevel-environment forms)  
     ;; 4 is initial empty slot on stack index 
  (assign-toplevel-stack-index (map (lambda (x) (car (cdr x)))
			   (toplevel-definitions forms))
		      4))



;; collect toplevel procedure arities
;; form = [define f [lambda X ...]]
;;           |            |
;;          car   cadr   caddr
;;                       
(define (toplevel-procedures forms)
  (cond
   ((null? forms) '())
   ((pair? (car forms))
    (let ((form (car forms)))
      (if (and (pair? form)
	       (eq? 'define (car form))
	       (symbol? (car (cdr form)))
	       (pair? (car (cdr (cdr form))))
	       (eq? 'lambda (car (car (cdr (cdr form))))))
	  (cons (list (car (cdr form))
		      (length (car (cdr (car (cdr (cdr form)))))))
		(toplevel-procedures (cdr forms)))
	  (toplevel-procedures (cdr forms)))))
   (else (toplevel-procedures (cdr forms)))))
	   
	   
(define (stage-2 forms)
  (toplevel-procedures (stage-1 forms)))


;;(pretty-print (stage-2 (read-file "/home/terry/lisp/demo/tak/tak.scm")))
;;(pretty-print (stage-1 (read-file "/home/terry/lisp/demo/tak/tak.scm")))

(define (stage-3 forms)
  (toplevel-definitions (stage-1 forms)))


;; arities and definitions listed first
;; compile definitions 
;; compile expressions
(define (stage-4 infile outfile)
  (let ((forms (stage-1 (read-file infile))))
    (stage-4b forms outfile)))


(define (stage-4b forms outfile)
    (let ((arity (toplevel-procedures forms))
	  (def-forms (toplevel-definitions forms))
	  (non-def-forms (toplevel-non-definitions forms))
	  (top-environment (toplevel-environment forms)))
      (let ((last-stack-index (next-toplevel-stack-index-available top-environment)))
	

      (newline)
      (display "stage-4b: definitions:")
      (newline)
      (map (lambda (of) (display of) (newline)) def-forms)
      (newline)

      (newline)
      (display "stage-4b: non definitions:")
      (newline)
      (map (lambda (of) (display of) (newline)) non-def-forms)
      (newline)

      
      (newline)
      (display "stage-4b: top environment:")
      (newline)
      (map (lambda (of) (display of) (newline)) top-environment)
      (newline)

      (newline)
      (display "stage-4b: first open stack index = :")
      (display last-stack-index)
      (newline)
      
      (call-with-output-file outfile
	(lambda (p)
	  (set-emit-output-port! p)

	  (emit "")
	  (emit "extern debug_stack")
	  (emit "global scheme_entry")

	  
	  ;; here compile the expression
	  ;; initial stack index is negative wordsize
	  ;; as [ esp - 4 ] , since esp holds return address.
	  (let ((initial-environment top-environment)
		(stack-index (- (* 2 word)))) ;; -8

	    ;; sufficiently large toplevel
	    (emit "section .data")
	    (emit "align 32")	    
	    (emit "toplevel: times " (+ (length def-forms) 4)   " dd 0")
	    
	    
	    (emit "section .text")
	    (emit "align 32")
	    (emit "scheme_entry: nop ")

	    ;; NO ENTRY PROLOGUE people !! 
	    ;;(emit "enter")	    
	    (emit "push ebp")
	    (emit "mov	ebp, esp")
	    
	    ;;(emit "sub esp, N")
	    
	    ;; HEAP is passed as 1st argument
	    ;; ebp + 8 = HEAP ptr passed by C
	    ;; ebp + 4 = RET ip
	    ;; ebp     = old EBP
	    (emit "mov dword esi , [ esp + 8 ] ")
	    (emit "scheme_heap_in_esi: nop")
	    ;; esp - 4 = first FREE slot on stack    

	    
	    
	    (map (lambda (expr)
		   (begin
		     (display "compiling expr => ")
		     (display expr)
		     (newline)		   
		     (comp expr stack-index initial-environment)))
		 def-forms)

	    (map (lambda (expr)
		   (begin
		     (display "compiling expr => ")
		     (display expr)
		     (newline)		   
		     (comp expr stack-index initial-environment)))
		 non-def-forms)

	    
	    ;; NO EXIT PROLOGUE people !! 
	    (emit "mov	esp, ebp")
	    (emit "pop	ebp")

	    
	    ;; final return 
	    (emit "ret")))))))





;; 
;;(stage-4 "/home/terry/lisp/demo/tak/tak.scm" "entry.asm")




	    












