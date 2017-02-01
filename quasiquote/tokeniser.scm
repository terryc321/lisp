
;;
;;
;; simple textual tokeniser
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

;; dont do anything
(define list->str list->string)


(define character-comma #\, )
(define character-atsign #\@ )
(define character-open-parens #\( )
(define character-close-parens #\) )
(define character-quote #\' )
(define character-backquote #\` )


(define letter? (lambda (c)
		  (or
		   (and (>= (char->integer c) (char->integer #\a))
			(<= (char->integer c) (char->integer #\z)))
		   (and (>= (char->integer c) (char->integer #\A))
			(<= (char->integer c) (char->integer #\Z)))
		   (and (>= (char->integer c) (char->integer #\0))
			(<= (char->integer c) (char->integer #\9)))
		   (char=? c #\+ )
		   (char=? c #\- )
		   (char=? c #\_ )
		   (char=? c #\= )
		   (char=? c #\* )
		   (char=? c #\& )
		   (char=? c #\^ )
		   (char=? c #\% )
		   (char=? c #\$ )
		   (char=? c #\! )
		   (char=? c #\? )
		   (char=? c #\. )
		   (char=? c #\< )
		   (char=? c #\> )
		   (char=? c #\~ ))))

(define whitespace? (lambda (c) (<= (char->integer c) 32)))
			    

;; tokenises a list of characters
;; (tokenise (str->list "(+ 1 2 )"))
(define (tokenise xs)
  (cond
   ((string? xs)
    (tokenise (str->list xs)))
   ((null? xs) xs)
   ((pair? xs)
    (cond
     ((char=? (car xs) character-comma)  (tokenise-comma xs))
     ((char=? (car xs) character-open-parens)  (tokenise-open-parens xs))
     ((char=? (car xs) character-close-parens)  (tokenise-close-parens xs))     
     ((char=? (car xs) character-quote)  (tokenise-quote xs))     
     ((char=? (car xs) character-backquote)  (tokenise-backquote xs))     
     ((letter? (car xs)) (tokenise-word xs))
     (else (tokenise (cdr xs)))))
   (else (error "tokenise expects either string or a list of characters"))))



(define (tokenise-comma xs)
  (cond
   ((null? (cdr xs))
    (cons (cons 'symbol 'comma)
	  '()))
   ((char=? (cadr xs) character-atsign)
    (cons (cons 'symbol 'comma-at)
	  (tokenise (cddr xs))))
   (else  (cons (cons 'symbol 'comma)
		(tokenise (cdr xs))))))
   
(define (tokenise-open-parens xs)
  (cons (cons 'symbol 'open-parens)
	(tokenise (cdr xs))))

(define (tokenise-close-parens xs)
  (cons (cons 'symbol 'close-parens)
	(tokenise (cdr xs))))

(define (tokenise-quote xs)
  (cons (cons 'symbol 'quote)
	(tokenise (cdr xs))))

(define (tokenise-backquote xs)
  (cons (cons 'symbol 'backquote)
	(tokenise (cdr xs))))

(define (tokenise-word xs)
  (tokenise-word-helper xs '()))


(define (tokenise-word-helper xs word)
  (cond
   ((or (null? xs)
	(not (letter? (car xs))))
    (cons (cons 'word (list->str (reverse word)))
	  (tokenise xs)))
   (else (tokenise-word-helper (cdr xs) (cons (car xs) word)))))


(define file->str (lambda (filename)
		    (let ((port (open-input-file filename)))
		      (let ((result (file->str-loop port '())))
			(close-port port)
			result))))

(define file->str-loop (lambda (port xs)
			 (let ((obj (read-char port)))
			   (cond
			    ((eof-object? obj) (reverse xs))
			    (else (file->str-loop port (cons obj xs)))))))


;;
;; simple recursive parser for s expression syntax
;;
;; simple tokeniser
;;
;; (backquote x) `
;; (quote x)   ' 
;; (unquote x)  also known as comma ,
;; (splice x)   also known as comma-atsign ,@
;;
;; parse tokens ts
;;
(define parse-stack '())
(define parse-tokens '())

(define (open-parens? xs)
  (and (pair? xs)
       (eq? (car xs) 'symbol)
       (eq? (cdr xs) 'open-parens)))

(define (close-parens? xs)
  (and (pair? xs)
       (eq? (car xs) 'symbol)
       (eq? (cdr xs) 'close-parens)))




(define (parse-atom)
  (cond
   ((null? parse-tokens)
    (display "parse-atom : expected some tokens ")
    (newline)    
    (error "parse-atom : not enough tokens"))
   (else
    (display "parse-atom : atom = ")
    (display (car parse-tokens))
    (newline)    
    (set! parse-stack (cons (car parse-tokens) parse-stack))
    (set! parse-tokens (cdr parse-tokens)))))




(define (parse-list-loop xs)
  (display "parse-list-loop : xs =")
  (display xs)
  (newline)
  
  (display "parse-list-loop : calling parse .")
  (newline)
  (parse)  
  (cond
   
   ((null? parse-tokens) ;; no tokens ??    
    (display "parse-list-loop : no more tokens .")
    (newline))
   
   ((close-parens? (car parse-tokens)) ;; end of list
    (set! parse-tokens (cdr parse-tokens))
    (set! parse-stack (cons (cons 'list (reverse xs)) parse-stack)))
   
   (else ;; more items to process
    (begin
      (let ((val (car parse-stack)))
	(set! parse-stack (cdr parse-stack))
	(parse-list-loop (cons val xs)))))))



      
(define (parse-list)
  (cond
   ((null? parse-tokens)
    (display "parse-list : no tokens ?? - done")
    (newline))
   ((close-parens? (car parse-tokens)) ;; end of list ie () 
    (set! parse-tokens (cdr parse-tokens))
    (set! parse-stack (cons (cons 'list 'nil) parse-stack)))
   (else 
    (set! parse-tokens (cdr parse-tokens))
    (parse-list-loop '()))))


(define (parse)
  (cond
   ((null? parse-tokens)
    (display "parse: no more tokens - done")
    (newline))
   ((open-parens? (car parse-tokens))
    (display "parse: found open-parens")
    (newline)
    (parse-list))
   ((close-parens? (car parse-tokens))
    (display "parse: found close-parens")
    (newline))
   (else
    (display "parse: calling default parse-atom")
    (newline)
    (parse-atom))))




(define (parse-entry ts)
  (set! parse-stack '())
  (set! parse-tokens ts)
  (parse)
  (car parse-stack))


  


  
  
	      


