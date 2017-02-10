
(define naive-expand
  (lambda (form)
    (if (not (pair? form))
	form
	(case (car form)
	  ((quote) form)
	  ((lambda) `(lambda ,(cadr form)
		  ,@(map naive-expand (cddr form))))
	  ((set!) `(set! ,(cadr form) 
		     ,(naive-expand (caddr form))))
	  ((if) `(if ,@(map naive-expand (cdr form))))
	  (else (if (macro-tag? (car form))
		    (naive-expand 
		     ((macro-transformer (car form)) form))
		    (map naive-expand form)))))))

(define *macro-transformers* '())

(define macro-tag?
  (lambda (tag)
    (and (symbol? tag)
	 (assq tag *macro-transformers*))))

(define macro-transformer
  (lambda (tag)
    (cdr (assq tag *macro-transformers*))))

(define macro-transformer-set!
  (lambda (tag transformer)
    (let ((pair (assq tag *macro-transformers*)))
      (if pair
	  (set-cdr! pair transformer)
	  (set! *macro-transformers*
	    (cons (cons tag transformer) 
		  *macro-transformers*))))))



(macro-transformer-set!
 'push                                 ; (push a l)
 (lambda (form)                        ; ==> (set! l (cons a l))
   `(set! ,(caddr form)
      (cons ,(cadr form) ,(caddr form)))))


(macro-transformer-set!
 'let                                  ; (let ((var init) ...)
 (lambda (form)                        ;   body ...)
   `((lambda ,(map car (cadr form))    ; ==> ((lambda (var ...)
       ,@(cddr form))                 ;        body ...)
     ,@(map cadr (cadr form)))))       ;      init ...)


(macro-transformer-set!
 'or
 (lambda (form)
   (cond
    ((null? (cdr form))               ; (or)
     '#f)                             ; ==> #f
    ((null? (cddr form))              ; (or e)
     (cadr form))                     ; ==> e
    (else                             ; (or e1 e2 ...)
     `((lambda (temp)                 ; ==> ((lambda (temp)
	 (if temp                     ;        (if temp
	     temp                     ;            temp
	     (or ,@(cddr form))))     ;            (or e2 ...)))
       ,(cadr form))))))              ;      e1)


(macro-transformer-set!
 'catch
 (lambda (form)
   `(call-with-current-continuation
     (lambda (,(capture 'throw))
       ,(cadr form)))))



;;; kohlbecker algorithm
;; ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/misc/macros-02.txt

(define *top-mark* #f)

(define kohlbecker-expand
  (lambda (form)
    (set! *top-mark* (make-new-mark))
    (unmark
     (alpha-convert
      (expand-marked
       (mark form *top-mark*))))))

(define capture
  (lambda (var)
    (make-identifier var *top-mark*)))


;; version without variable capture
;; (define kohlbecker-expand
;;   (lambda (form)
;;     (unmark
;;      (alpha-convert
;;       (expand-marked
;;        (mark form (make-new-mark)))))))


(define mark
  (lambda (s stamp-value)
    (cond
     ((or (identifier? s)
	  (macro-tag? s)
	  (reserved-word? s)) s)
     ((symbol? s) (make-identifier s stamp-value))
     ((pair? s) (map (lambda (z) (mark z stamp-value)) s))
     (else s))))

(define reserved-word?
  (lambda (x) (memq x '(quote lambda set! if))))

(define expand-marked
  (lambda (mform)
    (if (not (pair? mform))
	mform
	(case (car mform)
	  ((quote) mform)
	  ((lambda) `(lambda ,(cadr mform)
		  ,@(map expand-marked (cddr mform))))
	  ((set!) `(set! ,(cadr mform)
		     ,(expand-marked (caddr mform))))
	  ((if) `(if ,@(map expand-marked (cdr mform))))
	  (else (if (macro-tag? (car mform))
		    (expand-marked
		     (mark ((macro-transformer (car mform)) mform)
			   (make-new-mark)))
		    (map expand-marked mform)))))))



(define alpha-convert
  (lambda (mform)
    (if (not (pair? mform))
	mform
	(case (car mform)
	  ((lambda) (alpha-convert-lambda
		(cadr mform)
		(map alpha-convert (cddr mform))))
	  ((quote) (unmark mform))
	  ((set!) `(set! ,(cadr mform)
		     ,(alpha-convert (caddr mform))))
	  ((if) `(if ,@(map alpha-convert (cdr mform))))
	  (else (map alpha-convert mform))))))

(define alpha-convert-lambda
  (lambda (varlist body)
    (let ((substitutions (make-substitution-info varlist)))
      `(lambda ,(perform-substitutions substitutions varlist)
	 ,@(perform-substitutions substitutions body)))))

(define make-substitution-info
  (lambda (varlist)
    (map (lambda (id)
	   (cons id
		 (identifier->unique-symbol id)))
	 varlist)))

(define perform-substitutions
  (lambda (substitutions s)
    (cond
     ((identifier? s)
      (let ((entry (assoc-id s substitutions)))
	(if entry
	    (cdr entry)
	    s)))
     ((not (pair? s))
      s)
     (else
      (cons (perform-substitutions substitutions (car s))
	    (perform-substitutions substitutions (cdr s)))))))


(define unmark
  (lambda (s)
    (cond
     ((identifier? s) (identifier->symbol s))
     ((pair? s) (map unmark s))
     (else s))))




(define make-new-mark
  (let ((v 0))
    (lambda ()
      (begin (set! v (+ v 1))
	     v))))

(define mark=? =)


(define make-identifier
  (lambda (name mark)
    (vector 'IDENTIFIER name mark)))

(define identifier->symbol
  (lambda (x) (vector-ref x 1)))

(define identifier-mark
  (lambda (x) (vector-ref x 2)))

(define identifier?
  (lambda (x)
    (and (vector? x)
	 (= (vector-length x) 3)
	 (eq? (vector-ref x 0) 'IDENTIFIER))))

(define identifier=?
  (lambda (x y)
    (and (identifier? x)
	 (identifier? y)
	 (eq? (identifier->symbol x) (identifier->symbol y))
	 (mark=? (identifier-mark x) (identifier-mark y)))))

(define assoc-id
  (lambda (id s)
    (cond
     ((null? s) #f)
     ((identifier=? id (caar s)) (car s))
     (else (assoc-id id (cdr s))))))

(define identifier->unique-symbol
  (lambda (x)
    (gensym)))

;; comment out this identifier->unique-symbol -- its only for education purposes
;; it DOES NOT generate unique symbols !!
;; (define identifier->unique-symbol
;;   (lambda (x)
;;     (string->symbol
;;      (string-append (symbol->string (identifier->symbol x))
;; 		    ":"
;; 		    (number->string (identifier-mark x))))))

(define demo0 (kohlbecker-expand
	       '(let ((temp 37.0))
		  (or (foo temp)
		      temp))))

(define demo1 (kohlbecker-expand
	       '(let ((cons '()))
		  (push "ghengis" cons)
		  (push "khubla" cons)
		  cons)))



