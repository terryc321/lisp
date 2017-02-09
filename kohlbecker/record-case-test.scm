#lang racket

(require compatibility/defmacro)


(defmacro record-case args    ;; args = (record-exp . clauses)
(let ((record-exp (car args))
      (clauses (cdr args)))
  (let ((var     (gensym)))
    ;; make-conds expands all the record-case clauses into a
    ;; list of appropriate cond style clauses, i.e. each clause
    ;; list of the form:
    ;;    (name-i field-list-i consequent-i)
    ;; gets expanded into:
    ;;    ((name-i? record-exp)
    ;;        (let ((field-list-i-1 field-name-1)
    ;;              (           ...             )
    ;;              (field-list-i-n field-name-n))
    ;;          consequent-i))
    (letrec ((make-conds
             (lambda (clause)
               (cond
                ((null? clause)
                 `((else (error 'record-case "no clause match: ~a" ,var))))
                ((eq? (caar clause) 'else)
                 `((else ,@(cdar clause))))
                (else
    ;; The make-lets procedure returns the let expression bindings for
    ;; one record-case clause only. These in turn will be combined with
    ;; the (pred? value) list to form one clause of the cond statement.
                 (letrec ((make-lets
                           (lambda (fields get-field)
                             (if (null? fields)
                               '()
                               (cons
                                `(,(car fields) (car ,get-field))
                                (make-lets (cdr fields)
                                           (list 'cdr get-field)))))))
                   (let* ((key (caar clause))
                          (pred? (if (list? key)
                                   `(memv (car ,var) ',key)
                                   `(eqv? (car ,var) ',key)))
                          (lets (make-lets (cadar clause) `(cdr ,var))))
                     (cons
                      `(,pred? (let ,lets ,@(cddar clause)))
                      (make-conds (cdr clause))))))))))
      (if #t ;;(record-macros:check-syntax clauses)
        `(let ((,var ,record-exp)) (cond ,@(make-conds clauses)))
        ;; this error call should never be processed since
        ;; check-syntax only returns #t or doesn't return at all
        (error 'record-case-macro
          "Unspecified error ~%Record-exp = ~p~%Clauses = ~p"
          record-exp clauses))))))


;;; load recscm.scm

(define calc
     (lambda (x)
       (if (integer? x)
	   x
	   (record-case x
			(+ (x y) (+ (calc x) (calc y)))
			(* (x y) (* (calc x) (calc y)))
			(- (x) (- 0 (calc x)))
			(else (error "invalid expression"))))))



