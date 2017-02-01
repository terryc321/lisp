
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



(define-syntax when
  (syntax-rules ()
    ((when condition body ...)
     (if condition (begin body ...) #f))))




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



;; need to make objects so we dont accidently slap a cons into a structure
;;

(define *debug-args->assoc* #f)

(define (args->assoc xs)
  (when *debug-args->assoc* 
    (display "args->assoc :")
    (display xs)
    (newline))
  (cond
   ((null? xs) xs)
   ((and (pair? xs)
	 (pair? (cdr xs))
	 (symbol? (car xs)))
    (cons (list (car xs) (car (cdr xs)))
	  (args->assoc (cdr (cdr xs)))))
   (else
    (when *debug-args->assoc*
      (display "args->assoc err: ")
      (display xs)
      (newline))
    (error "malformed args association list" xs ))))





;;---------------------------------------------------------------------------------------------
;; object
(define make-comma (lambda constructs
		     (let ((line 0)			   
			   (col 0)
			   (constructor-args (args->assoc constructs)))
		       
		       (when *debug-args->assoc*
			 (display "alist => ")
			 (display constructor-args)
			 (newline))
		       
		       (if (assoc 'line constructor-args)
			   (set! line (car (cdr (assoc 'line constructor-args))))
			   #f)
		       (if (assoc 'col constructor-args)
			   (set! col (car (cdr (assoc 'col constructor-args))))
			   #f)		       
		       (lambda usage
			 (cond
			  ((null? usage) #f)
			  ((eq? (car usage) 'type) 'comma)
			  ((eq? (car usage) 'line) line)
			  ((eq? (car usage) 'col) col)
			  ((eq? (car usage) 'loc) (cons line col))
			  ((eq? (car usage) 'string) ",")
			  (else (list (list 'constructor-args constructor-args)
				      (list 'usage usage))))))))


(define make-comma-atsign (lambda constructs
		     (let ((line 0)			   
			   (col 0)
			   (constructor-args (args->assoc constructs)))
		       
		       (when *debug-args->assoc*
			 (display "alist => ")
			 (display constructor-args)
			 (newline))
		       
		       (if (assoc 'line constructor-args)
			   (set! line (car (cdr (assoc 'line constructor-args))))
			   #f)
		       (if (assoc 'col constructor-args)
			   (set! col (car (cdr (assoc 'col constructor-args))))
			   #f)		       
		       (lambda usage
			 (cond
			  ((null? usage) #f)
			  ((eq? (car usage) 'type) 'comma-atsign)
			  ((eq? (car usage) 'line) line)
			  ((eq? (car usage) 'col) col)
			  ((eq? (car usage) 'loc) (cons line col))
			  ((eq? (car usage) 'string) ",@")
			  (else (list (list 'constructor-args constructor-args)
				      (list 'usage usage))))))))


(define make-quote
  (lambda constructs
    (let ((line 0)			   
	  (col 0)
	  (constructor-args (args->assoc constructs)))
      
      (when *debug-args->assoc*
	(display "alist => ")
	(display constructor-args)
	(newline))
      
      (if (assoc 'line constructor-args)
	  (set! line (car (cdr (assoc 'line constructor-args))))
	  #f)
      (if (assoc 'col constructor-args)
	  (set! col (car (cdr (assoc 'col constructor-args))))
	  #f)		       
      (lambda usage
	(cond
	 ((null? usage) #f)
	 ((eq? (car usage) 'type) 'quote)
	 ((eq? (car usage) 'line) line)
	 ((eq? (car usage) 'col) col)
	 ((eq? (car usage) 'loc) (cons line col))
	 ((eq? (car usage) 'string) ",")
	 (else (list (list 'constructor-args constructor-args)
		     (list 'usage usage))))))))



(define make-backquote
  (lambda constructs
    (let ((line 0)			   
	  (col 0)
	  (constructor-args (args->assoc constructs)))
      
      (when *debug-args->assoc*
	(display "alist => ")
	(display constructor-args)
	(newline))
      
      (if (assoc 'line constructor-args)
	  (set! line (car (cdr (assoc 'line constructor-args))))
	  #f)
      (if (assoc 'col constructor-args)
	  (set! col (car (cdr (assoc 'col constructor-args))))
	  #f)		       
      (lambda usage
	(cond
	 ((null? usage) #f)
	 ((eq? (car usage) 'type) 'backquote)
	 ((eq? (car usage) 'line) line)
	 ((eq? (car usage) 'col) col)
	 ((eq? (car usage) 'loc) (cons line col))
	 ((eq? (car usage) 'string) "`")
	 (else (list (list 'constructor-args constructor-args)
		     (list 'usage usage))))))))




(define make-open-parens
  (lambda constructs
    (let ((line 0)			   
	  (col 0)
	  (constructor-args (args->assoc constructs)))
      
      (when *debug-args->assoc*
	(display "alist => ")
	(display constructor-args)
	(newline))
      
      (if (assoc 'line constructor-args)
	  (set! line (car (cdr (assoc 'line constructor-args))))
	  #f)
      (if (assoc 'col constructor-args)
	  (set! col (car (cdr (assoc 'col constructor-args))))
	  #f)		       
      (lambda usage
	(cond
	 ((null? usage) #f)
	 ((eq? (car usage) 'type) 'open-parens)
	 ((eq? (car usage) 'line) line)
	 ((eq? (car usage) 'col) col)
	 ((eq? (car usage) 'loc) (cons line col))
	 ((eq? (car usage) 'string) "(")
	 (else (list (list 'constructor-args constructor-args)
		     (list 'usage usage))))))))



(define make-close-parens
  (lambda constructs
    (let ((line 0)			   
	  (col 0)
	  (constructor-args (args->assoc constructs)))
      
      (when *debug-args->assoc*
	(display "alist => ")
	(display constructor-args)
	(newline))
      
      (if (assoc 'line constructor-args)
	  (set! line (car (cdr (assoc 'line constructor-args))))
	  #f)
      (if (assoc 'col constructor-args)
	  (set! col (car (cdr (assoc 'col constructor-args))))
	  #f)		       
      (lambda usage
	(cond
	 ((null? usage) #f)
	 ((eq? (car usage) 'type) 'close-parens)
	 ((eq? (car usage) 'line) line)
	 ((eq? (car usage) 'col) col)
	 ((eq? (car usage) 'loc) (cons line col))
	 ((eq? (car usage) 'string) ")")
	 (else (list (list 'constructor-args constructor-args)
		     (list 'usage usage))))))))





(define make-word
  (lambda constructs
    (let ((line 0)			   
	  (col 0)
	  (word "#unspecified")
	  (constructor-args (args->assoc constructs)))
      
      (when *debug-args->assoc*
	(display "alist => ")
	(display constructor-args)
	(newline))
      
      (when (assoc 'word constructor-args)
	(set! word (car (cdr (assoc 'word constructor-args)))))
      
      (when (assoc 'line constructor-args)
	(set! line (car (cdr (assoc 'line constructor-args)))))
      
      (when (assoc 'col constructor-args)
	(set! col (car (cdr (assoc 'col constructor-args)))))
      
      (lambda usage
	(cond
	 ((null? usage) #f)
	 ((eq? (car usage) 'type) 'word)
	 ((eq? (car usage) 'line) line)
	 ((eq? (car usage) 'col) col)
	 ((eq? (car usage) 'loc) (cons line col))
	 ((eq? (car usage) 'string) word)
	 ((eq? (car usage) 'word) word)
	 (else (list (list 'constructor-args constructor-args)
		     (list 'usage usage))))))))



;;-------------------------------------------------------------------------------
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
   ;; finishes with comma ,
   ((null? (cdr xs))
    (cons (make-comma) '()))
   ;; comma at ,@ 
   ((char=? (cadr xs) character-atsign)
    (cons (make-comma-atsign)
	  (tokenise (cddr xs))))
   ;; a comma symbol followed by stuff
   (else  (cons (make-comma) 
		(tokenise (cdr xs))))))

   
(define (tokenise-open-parens xs)
  (cons (make-open-parens)
	(tokenise (cdr xs))))

(define (tokenise-close-parens xs)
  (cons (make-close-parens) 
	(tokenise (cdr xs))))

(define (tokenise-quote xs)
  (cons (make-quote)
	(tokenise (cdr xs))))

(define (tokenise-backquote xs)
  (cons (make-backquote) 
	(tokenise (cdr xs))))

(define (tokenise-word xs)
  (tokenise-word-helper xs '()))


(define (tokenise-word-helper xs word)
  (cond
   ((or (null? xs)
	(not (letter? (car xs))))
    (cons (make-word 'word (list->str (reverse word)))
	  (tokenise xs)))
   (else (tokenise-word-helper (cdr xs) (cons (car xs) word)))))


;;-------------------------------------------------------------------------------
;; file to list of characters actually , but hey.
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

;;-------------------------------------------------------------------------------
(define show-tokens (lambda (tokens)
		      (map (lambda (object) (object 'string)) tokens)))



;;-------------------------------------------------------------------------------
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

;;-------------------------------------------------------------------------------------
;; since parse stack only has objects that work with our interface
(define (open-parens? obj)   (eq? (obj 'type) 'open-parens))
(define (close-parens? obj)  (eq? (obj 'type) 'close-parens))
(define (quote? obj)    (eq? (obj 'type) 'quote))
(define (comma? obj)   (eq? (obj 'type) 'comma))
(define (comma-atsign? obj)   (eq? (obj 'type) 'comma-atsign))
(define (backquote? obj)   (eq? (obj 'type) 'backquote))

;;-------------------------------------------------------------------------------------

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
  (display "Xparse-list-loop : xs =")
  (display xs)
  (newline)
  
  (display "Xparse-list-loop : calling parse .")
  (newline)
  (parse-recur)  
  (cond
   
   ((null? parse-tokens) ;; no tokens ??    
    (display "Xparse-list-loop : no more tokens .")
    (newline)
    (error "parse-list -loop : expected more tokens ."))   
   
   ((close-parens? (car parse-tokens)) ;; end of list
    (display "Xparse-list-loop : close parens .")
    (newline)
    (begin
      (let ((val (car parse-stack)))
	(set! parse-stack (cdr parse-stack))
	(set! parse-tokens (cdr parse-tokens))
	(set! parse-stack (cons (cons 'list (reverse (cons val xs))) parse-stack)))))
   
  (else ;; more items to process
    (display "Xparse-list-loop : more items to process .")
    (newline)
    (begin
      (let ((val (car parse-stack)))
	(set! parse-stack (cdr parse-stack))
	(parse-list-loop (cons val xs)))))))


      
(define (parse-list)
  (cond
   ((null? parse-tokens)
    (display "parse-list : no tokens ?? - done")
    (newline))
   ((close-parens? (car parse-tokens)) ;; end of list
    (display "Xparse-list : close parens ... just returning")
    (newline))
   (else
    (cond
     ((and (not (null? parse-tokens))
	   (not (null? (cdr parse-tokens)))
	   (open-parens? (car parse-tokens))
	   (close-parens? (car (cdr parse-tokens))))
      (display "parse-list : found () generating THE-EMPTY-LIST.")
      (newline)      
      (set! parse-stack (cons (cons 'empty-list 'nil)  parse-stack))
      (set! parse-tokens (cdr (cdr parse-tokens))))      
     (else
      (display "parse-list : calling parse-list-loop.")
      (newline)
      (set! parse-tokens (cdr parse-tokens))
      (parse-list-loop '()))))))



(define (parse-recur)
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

   ((quote? (car parse-tokens))
    (display "parse: found quote")
    (newline)
    (set! parse-tokens (cdr parse-tokens))
    (parse-recur)
    (let ((val (list 'quote (car parse-stack))))
      (set! parse-stack (cdr parse-stack))
      (set! parse-stack (cons val parse-stack))))

   ((backquote? (car parse-tokens))   
    (display "parse: found backquote")
    (newline)
    (set! parse-tokens (cdr parse-tokens))
    (parse-recur)
    (let ((val (list 'backquote (car parse-stack))))
      (set! parse-stack (cdr parse-stack))
      (set! parse-stack (cons val parse-stack))))

   ((comma? (car parse-tokens))
    (display "parse: found comma")
    (newline)
    (set! parse-tokens (cdr parse-tokens))
    (parse-recur)
    (let ((val (list 'comma (car parse-stack))))
      (set! parse-stack (cdr parse-stack))
      (set! parse-stack (cons val parse-stack))))
   
   ((comma-at? (car parse-tokens))
    (display "parse: found comma-at")
    (newline)
    (set! parse-tokens (cdr parse-tokens))
    (parse-recur)
    (let ((val (list 'comma-at (car parse-stack))))
      (set! parse-stack (cdr parse-stack))
      (set! parse-stack (cons val parse-stack))))
   
   (else
    (display "parse: calling default parse-atom")
    (newline)
    (parse-atom))))




(define (parse ts)
  (set! parse-stack '())
  (set! parse-tokens ts)
  (parse-recur)
  (car parse-stack))

;;------------------------------------------------------------------------------------
;; recursively show parse tree
;; guaranteed to be acyclic because we dont handle #1# ... notation yet.
;; cyclic lists need attentio tho.
  
(define (print-parse xs)
  (if
   (null? xs)
   (begin
     "()")
   (begin
     (let ((obj (car xs)))
       (cond
	((eq? (obj 'type) 'list)
	 (print-list xs))
	((eq? (obj 'type) 'word)
	 (print-word xs))
	((eq? (obj 'type) 'comma)
	 (print-comma xs))
	((eq? (obj 'type) 'comma-at)
	 (print-comma-at xs))
	((eq? (obj 'type) 'backquote)
	 (print-backquote xs))
	((eq? (obj 'type) 'quote)
	 (print-quote xs))
	(else
	 (error "dont know how to print this : " xs)))))))


(define (print-word xs)
  (display (cdr xs)))

(define (print-list xs)
  (display "(")
  (print-list-helper (cdr xs)))

(define (print-list-helper xs)
  (cond
   ((null? xs) #f)
   (else (print-parse (car xs))
	 (if (null? (cdr xs))
	     (begin (display ")"))
	     (begin (display " ")
		    (print-list-helper (cdr xs)))))))


(define (print-comma xs)
  (display "(comma ")
  (print-parse (car (cdr xs)))
  (display ")"))


(define (print-comma-at xs)
  (display "(comma-at ")
  (print-parse (car (cdr xs)))
  (display ")"))


(define (print-quote xs)
  (display "(quote ")
  (print-parse (car (cdr xs)))
  (display ")"))


(define (print-backquote xs)
  (display "(backquote ")
  (print-parse (car (cdr xs)))
  (display ")"))


;;
;; (print-parse (parse (tokenise "(1 `(,2) 3)")))   
;;

;;
;; tagged 
;;
(define (tag-backquote? x) (and (pair? x) (or (eq? (car x) 'backquote)
					      (eq? (car x) 'quasiquote))))
					      
(define (tag-comma? x) (and (pair? x)
			    (or (eq? (car x) 'comma)
				(eq? (car x) 'unquote))))
			    
(define (tag-comma-atsign? x) (and (pair? x)
				   (or (eq? (car x) 'comma-atsign)
				       (eq? (car x) 'unquote-splicing))))

(define (tag-data x) (and (pair? x) (car (cdr x))))


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


;;
;; recursively explore structure.
;;

;;
;; find quasiquotation or backquote , expand its contents
;;
(define (qq x)
  (cond
   ((tag-backquote? x)
    (qq-expand (tag-data x)))
   (else x)))


  
  
	      


