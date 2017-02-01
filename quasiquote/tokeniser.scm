;;
;;
;; simple textual tokeniser
;;
;;
;;
;;
;; identify tokens
;; 
;; open parens [(]
;; close parens [)]
;; quote [']
;; backquote [`]
;; comma [,]
;; comma-atsign [,@]
;;


;; (string-length "asdfmko")
;; (string-ref "asdf" 0)

;; string length
(define str-len string-length)


;; index starts from 1 
;; (str-ref 1 "asdf") = #\a
;; (str-ref 4 "asdf") = #\f
(define str-ref (lambda (n s) (string-ref s (- n 1))))

(define (str->list s)
  (define (str->list-helper s n lim xs)
    (cond
     ((> n lim) (reverse  xs))
     (else (str->list-helper s (+ n 1) lim (cons (str-ref n s) xs)))))  
  (str->list-helper s 1 (str-len s) '()))



;; tokenises a list of characters
;; (tokenise (str->list "(+ 1 2 )"))
(define (tokenise xs)
  (cond
   ((string? xs)
    (tokenise (str->list xs)))
   ((null? xs) xs)
   ((pair? xs)
    (cond
     ((and (char=? (car xs) #\,)
	   (not (null? (cdr xs)))
	   (char=? (cadr xs) #\@))
      (cons
       (cons 'symbol 'comma-at)
       (tokenise (cdr (cdr xs)))))
     ((and (char=? (car xs) #\,)
	   (cons
	    (cons 'symbol 'comma)
	    (tokenise (cdr xs(cdr xs))))))
     (else (tokenise (cdr xs)))))
   (else (error "tokenise expects either string or a list of characters"))))


