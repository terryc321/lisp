;;
;; quasi quote implementation
;;
;; http://repository.readscheme.org/ftp/papers/pepm99/bawden.pdf
;;
;; Appendix A : Expansion Algorithm
;;
;; Assumed that some more primitive Lisp parser
;; has already read in the quasiquotation to be expanded.
;; and has somehow tagged all the quasiquotation markup.
;; This primitive parser must supply the following four functions
;;
;; tag-backquote?
;; this predicate should be true of the result of reading a backquote (`)
;; followed by an s expression 
;;
;; tag-comma?
;; this predicate should be true of the result of reading a comma (,)
;; followed by an s expression
;;
;; tag-comma-atsign?
;; this predicate should be true of the result of reading a comma-atsign (,@)
;; followed by an s expression
;;
;; tag-data
;; this function should be applied to an object that
;; satisfies one of the previous three predicates
;; it will return the s expression that followed the quasiquotation markup
;;
;; The main entry point is the function qq-expand which should be applied to an
;; expression that immediately followed a backquote character.
;; ie the outermost backquote tag should be stripped off BEFORE qq-expand
;; is called.
;; 
;; Code created by qq-expand and qq-expand-list performs all list construction
;; by using either append or list.  It must NEVER use CONS.  This is important
;; in order to make nested quasiquotations using splicing work properly.

;; The code generated here is correct but inefficient.  In a real lisp
;; implementation some optimization would need to be done.  Care must be taken not
;; to perform any optimizations that alter the behaviour of nested splicing.
;;


(define (tag-backquote? x)
  (and (pair? x)
       (pair? (cdr x))
       (or (eq? (car x) 'backquote)
	   (eq? (car x) 'quasiquote))))

(define (tag-comma? x)
  (and (pair? x)
       (pair? (cdr x))
       (or (eq? (car x) 'comma)
	   (eq? (car x) 'unquote))))

(define (tag-comma-atsign? x)
  (and (pair? x)
       (pair? (cdr x))
       (or (eq? (car x) 'comma-atsign)
	   (eq? (car x) 'unquote-splicing))))
	   

;; retrieve the boxed object from within backquote , comma and comma-atsign s
;; 2nd item of list
(define (tag-data x)
  (car (cdr x)))

;;
;; bawden expansion algorithm
;;
(define (qq-expand x)
  (cond ((tag-comma? x)
	 (tag-data x))
	((tag-comma-atsign? x)
	 (error "illegal"))
	((tag-backquote? x)
	 (qq-expand
	  (qq-expand (tag-data x))))
	((pair? x)
	 `(append
	   ,(qq-expand-list (car x))
	   ,(qq-expand (cdr x))))
	(else `',x)))

(define (qq-expand-list x)
  (cond ((tag-comma? x)
	 `(list ,(tag-data x)))
	((tag-comma-atsign? x)
	 (tag-data x))
	((tag-backquote? x)
	 (qq-expand-list
	  (qq-expand (tag-data x))))
	((pair? x)
	 `(list
	   (append
	    ,(qq-expand-list (car x))
	    ,(qq-expand (cdr x)))))
	(else `'(,x))))

;; to expand a given quasi-quotation , strip off leading backquote
;; pass it to qq-expand for processing
(define (qq x)
  (cond
   ((tag-backquote? x)
    (qq-expand (tag-data x)))
   (else x)))



