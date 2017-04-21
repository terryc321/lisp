

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
;; 
(load 	  "/home/terry/lisp/macros/let.scm")

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
  (alpha-convert (macro-expand forms)))



(define (toplevel-definitions forms)
  (define (toplevel-helper forms defs non-defs)
    (cond
     ((null? forms) (append (reverse defs) (reverse non-defs)))
     ((pair? (car forms))
      (let ((form (car forms)))
	(if (and (pair? form)
		 (eq? 'define (car form))
		 (symbol? (car (cdr form))))
	    (toplevel-helper (cdr forms) (cons form defs) non-defs)
	    (toplevel-helper (cdr forms) defs (cons form non-defs)))))     
     (else (toplevel-helper (cdr forms) defs (cons (car forms non-defs))))))
  (toplevel-helper forms '() '()))



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
    (let ((arity (toplevel-procedures forms))
	  (ordered-forms (toplevel-definitions forms)))
      (call-with-output-file outfile
	(lambda (p)
	  (set-emit-output-port! p)

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

	    (map (lambda (expr)
		   (display "compiling expr => ")
		   (display expr)
		   (newline)
		   
		   (comp expr stack-index initial-environment))
		 ordered-forms)

	    ;; final return 
	    (emit "ret")))))))


;; 
;;(stage-4 "/home/terry/lisp/demo/tak/tak.scm" "entry.asm")




	    












