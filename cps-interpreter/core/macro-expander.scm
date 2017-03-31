
;;
;; simple macro expander
;; theres a cps-macro-expander floating around , see lisp in small pieces


;;--------------------------------------------------------------------------
;; hide actual expansion implementation inside a letrec if required
;;--------------------------------------------------------------------------
;;
;;
(define *macro-expanders* '())

(define (install-macro keyword procedure)
  (set! *macro-expanders* (cons (list keyword procedure)
				*macro-expanders*)))

(define (get-macro keyword)
  (assoc keyword *macro-expanders*))


(define (macro-expand expr)
   (display "macro expanding :")
   (display expr)
   (newline)  
  ;; symbol macros ?
  (if (not (pair? expr))
      expr
      (let ((keyword (car expr)))
	(cond
	 ((eq? keyword 'quote)
	  expr)
	 (else ;; try macro expanding it
	  (let ((expander (get-macro keyword)))
	    (if  (pair? expander)
		 (begin
		   (display "found macro expander : ")
		   (display expander)
		   (newline)
		   ;; test prevents infinitely calling macro expansion
		   (let ((original expr)
			 (new ((car (cdr expander)) expr)))
		     (if
		      (equal? original new)
		      new
		      (macro-expand ((car (cdr expander)) expr)))))
		 (begin		   
		   (map macro-expand expr)))))))))










