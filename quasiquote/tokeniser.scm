
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
(define make-comma
  (lambda constructs
    (let ((line 0)			   
	  (col 0)
	  (box #f)
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
      (if (assoc 'box constructor-args)
	  (set! box (car (cdr (assoc 'box constructor-args))))
	  #f)		             
      (lambda usage
	(cond
	 ((null? usage) #f)
	 ((eq? (car usage) 'type) 'comma)
	 ((eq? (car usage) 'line) line)
	 ((eq? (car usage) 'col) col)
	 ((eq? (car usage) 'box) box)	 
	 ((eq? (car usage) 'loc) (cons line col))
	 ((eq? (car usage) 'string) ",")
	 (else (list (list 'constructor-args constructor-args)
		     (list 'usage usage))))))))


(define make-comma-atsign
  (lambda constructs
    (let ((line 0)			   
	  (col 0)
	  (box #f)
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
      (if (assoc 'box constructor-args)
	  (set! box (car (cdr (assoc 'box constructor-args))))
	  #f)		       

      (lambda usage
	(cond
	 ((null? usage) #f)
	 ((eq? (car usage) 'type) 'comma-atsign)
	 ((eq? (car usage) 'line) line)
	 ((eq? (car usage) 'col) col)
	 ((eq? (car usage) 'box) box)	 
	 ((eq? (car usage) 'loc) (cons line col))
	 ((eq? (car usage) 'string) ",@")
	 (else (list (list 'constructor-args constructor-args)
		     (list 'usage usage))))))))


(define make-quote
  (lambda constructs
    (let ((line 0)			   
	  (col 0)
	  (box #f)
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
      
      (if (assoc 'box constructor-args)
	  (set! box (car (cdr (assoc 'box constructor-args))))
	  #f)
      
      (lambda usage
	(cond
	 ((null? usage) #f)
	 ((eq? (car usage) 'type) 'quote)
	 ((eq? (car usage) 'line) line)
	 ((eq? (car usage) 'col) col)
	 ((eq? (car usage) 'box) box)	 
	 ((eq? (car usage) 'loc) (cons line col))
	 ((eq? (car usage) 'string) ",")
	 (else (list (list 'constructor-args constructor-args)
		     (list 'usage usage))))))))



(define make-backquote
  (lambda constructs
    (let ((line 0)			   
	  (col 0)
	  (box #f)
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
      (if (assoc 'box constructor-args)
	  (set! box (car (cdr (assoc 'box constructor-args))))
	  #f)
      (lambda usage
	(cond
	 ((null? usage) #f)
	 ((eq? (car usage) 'type) 'backquote)
	 ((eq? (car usage) 'line) line)
	 ((eq? (car usage) 'col) col)
	 ((eq? (car usage) 'box) box)	 
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


(define make-empty-list
  (lambda constructs
    (let ((line 0)			   
	  (col 0)
	  (constructor-args (args->assoc constructs)))
      
      (when *debug-args->assoc*
	(display "alist => ")
	(display constructor-args)
	(newline))
      
      (when (assoc 'line constructor-args)
	(set! line (car (cdr (assoc 'line constructor-args)))))
      
      (when (assoc 'col constructor-args)
	(set! col (car (cdr (assoc 'col constructor-args)))))
      
      (lambda usage
	(cond
	 ((pair? usage)
	  (cond
	   ((eq? (car usage) 'type) 'the-empty-list)
	   ((eq? (car usage) 'line) line)
	   ((eq? (car usage) 'col) col)
	   ((eq? (car usage) 'loc) (cons line col))
	   ((eq? (car usage) 'string) "()")
	   (else (error "make-empty-list") usage)))
	 (else (error "make-empty-list") usage))))))




(define make-cons-pair
  (lambda constructs
    (let ((line 0)			   
	  (col 0)
	  (hd #f)
	  (tl #f)
	  (constructor-args (args->assoc constructs)))
      
      (when *debug-args->assoc*
	(display "alist => ")
	(display constructor-args)
	(newline))
      
      (when (assoc 'line constructor-args)
	(set! line (car (cdr (assoc 'line constructor-args)))))
      
      (when (assoc 'col constructor-args)
	(set! col (car (cdr (assoc 'col constructor-args)))))

      (when (assoc 'hd constructor-args)
	(set! hd (car (cdr (assoc 'hd constructor-args)))))
      
      (when (assoc 'tl constructor-args)
	(set! tl (car (cdr (assoc 'tl constructor-args)))))
            
      (lambda usage
	(cond
	 ((null? usage) #f)
	 ((eq? (car usage) 'type) 'cons-pair)
	 ((eq? (car usage) 'hd) hd)
	 ((eq? (car usage) 'tl) tl)
	 ((eq? (car usage) 'line) line)
	 ((eq? (car usage) 'col) col)
	 ((eq? (car usage) 'loc) (cons line col))
	 ((eq? (car usage) 'string) "()")
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
(define (cons-pair? obj)   (eq? (obj 'type) 'cons-pair))
(define (empty-list? obj)   (eq? (obj 'type) 'the-empty-list))



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


(define (objectify-list xs)
  (cond
   ((null? xs) (make-empty-list))
   (else (make-cons-pair 'hd (car xs) 'tl (objectify-list (cdr xs))))))



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
	(set! parse-stack (cons (objectify-list (reverse (cons val xs))) parse-stack)))))
   
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
      (set! parse-stack (cons (make-empty-list)  parse-stack))
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
    (let ((val (make-quote 'box (car parse-stack))))
      (set! parse-stack (cdr parse-stack))
      (set! parse-stack (cons val parse-stack))))

   ((backquote? (car parse-tokens))   
    (display "parse: found backquote")
    (newline)
    (set! parse-tokens (cdr parse-tokens))
    (parse-recur)
    (let ((val (make-backquote 'box (car parse-stack))))
      (set! parse-stack (cdr parse-stack))
      (set! parse-stack (cons val parse-stack))))

   ((comma? (car parse-tokens))
    (display "parse: found comma")
    (newline)
    (set! parse-tokens (cdr parse-tokens))
    (parse-recur)
    (let ((val (make-comma 'box (car parse-stack))))
      (set! parse-stack (cdr parse-stack))
      (set! parse-stack (cons val parse-stack))))
   
   ((comma-atsign? (car parse-tokens))
    (display "parse: found comma-at")
    (newline)
    (set! parse-tokens (cdr parse-tokens))
    (parse-recur)
    (let ((val (make-comma-atsign 'box (car parse-stack))))
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
  
(define (print-parse obj)
  (cond
   ((eq? (obj 'type) 'the-empty-list)    (print-empty-list obj))
   ((eq? (obj 'type) 'cons-pair)         (print-cons-pair obj))
   ((eq? (obj 'type) 'word)              (print-word obj))   
   ((eq? (obj 'type) 'comma)             (print-comma obj))   
   ((eq? (obj 'type) 'comma-atsign)      (print-comma-atsign obj))
   ((eq? (obj 'type) 'backquote)         (print-backquote obj))
   ((eq? (obj 'type) 'quote)             (print-quote obj))
   (else
    (error "print-parse : error - how print this ?? : " obj))))

(define (print-empty-list obj)
  (display "()"))

(define (print-word obj)
  (display (obj 'word)))

;;------------------------------------------------------------------------------------
;; print cons pairs 
(define (print-cons-pair obj)
  (display "(")
  (print-cons-pair-helper obj))

(define (print-cons-pair-helper obj)
  (cond
   ;; reached end of list
   ((empty-list? obj) #f)
   ;; print the head of the list
   (else (print-parse (obj 'hd))
	 (if (empty-list? (obj 'tl))
	     (begin (display ")"))
	     (begin (display " ")
		    (print-cons-pair-helper (obj 'tl)))))))

;;------------------------------------------------------------------------------------
(define (print-comma obj)
  (display "(comma ")
  (print-parse (obj 'box))
  (display ")"))


(define (print-comma-atsign obj)
  (display "(comma-atsign ")
  (print-parse (obj 'box))
  (display ")"))


(define (print-quote obj)
  (display "(quote ")
  (print-parse (obj 'box))
  (display ")"))


(define (print-backquote obj)
  (display "(backquote ")
  (print-parse (obj 'box))
  (display ")"))


;; --------------------------------------------------------------------------------------
;; de- objectify the parse tree , so treat it as lisp s expressions
;;

;; snarf reader to convert numbers to numbers , symbols to symbols.
(define (convert-parse obj)
  (cond
   ((eq? (obj 'type) 'the-empty-list)    '())
   ((eq? (obj 'type) 'cons-pair)         (cons (convert-parse (obj 'hd))
					       (convert-parse (obj 'tl))))
   ((eq? (obj 'type) 'word)    (with-input-from-string (obj 'word) read))
   ((eq? (obj 'type) 'comma)             (list 'comma (convert-parse (obj 'box))))
   ((eq? (obj 'type) 'comma-atsign)      (list 'comma-atsign (convert-parse (obj 'box))))
   ((eq? (obj 'type) 'backquote)         (list 'backquote (convert-parse (obj 'box))))
   ((eq? (obj 'type) 'quote)             (list 'quote (convert-parse (obj 'box))))
   (else
    (error "convert-parse error !!! - how convert this ?? : " obj))))


;;----------------------------------------------------------------------------------------
;;
;; (print-parse (parse (tokenise "(1 `(,2) 3)")))   
;;

;;
;; because using poor mans closure objects
;; we can just message pass to object itself
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

;;
;; recursively explore structure.
;;


;; to expand a given quasi-quotation , strip off leading backquote
;; pass it to qq-expand for processing
(define (qq x)
  (cond
   ((tag-backquote? x)
    (qq-expand (tag-data x)))
   (else x)))


;; 
;;
(define f1 (eval (qq (convert-parse (parse (tokenise "`(1 (2 3) (4 (5)) 6)")))) (the-environment)))

;;
;; ------------------------------------------------------------------------
;; simple a - list of macro expanders
;;
;; macro expander
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
  (if (not (pair? expr))
      expr
      (let ((keyword (car expr)))
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
		 (map macro-expand expr)))))))


;; (define (macro-expand expr)
;;   (display "macro expanding :")
;;   (display expr)
;;   (newline)
;;   (if (not (pair? expr))
;;       expr
;;       (let ((keyword (car expr)))
;; 	(cond
;; 	 ;;((equal? keyword 'quote) expr)
;; 	 ;; ((equal? keyword 'lambda)
;; 	 ;;  (append
;; 	 ;;   (list 'lambda (cadr expr))
;; 	 ;;   (map macro-expand (cddr expr))))
;; 	 (else
;; 	  (let ((expander (get-macro keyword)))
;; 	    (if  (pair? expander)
;; 		 (begin
;; 		   (display "found macro expander : ")
;; 		   (display expander)
;; 		   (newline)
;; 		   (let ((original expr)
;; 			 (new ((car (cdr expander)) expr)))
;; 		     (if
;; 		      (equal? original new)
;; 		      new
;; 		      (macro-expand ((car (cdr expander)) expr)))))
;; 		 (begin		   
;; 		   (map macro-expand expr)))))))))

;;--------------------------------------------------------------------------
;; hide actual expansion implementation inside a letrec if required
;;--------------------------------------------------------------------------

;; quote expander
(install-macro 'quote  (lambda (expr) expr))

;; lamb-da expander
(install-macro 'lambda (lambda (expr)
		    (append (list 'lambda (cadr expr))
			    (map macro-expand (cddr expr)))))



;; install quasiquote expander
;;
(install-macro 'quasiquote qq)
(install-macro 'backquote qq)

;;    ((when condition body ...)
;;     (if condition (begin body ...) #f))))


;; when --> if
(install-macro
 'when
 (lambda (expr)
   `(if ,(car (cdr expr))
	(begin ,@(cddr expr))
	#f)))

;; different when macro that just expands its terms 
;; (install-macro 'when
;; 	       (lambda (expr)
;; 		 (append (list 'when)
;; 			 (map macro-expand (cdr expr)))))


;;
;; install COND macro
;; assuming a well formed cond macro ?
;; (cond (c1 e1a e1b e1c)(ELSE else1 else2 else3))
;; (if c1 (begin e1a e1b e1c) ...)
;;
;; install cond macro
(install-macro
 'cond
 (lambda (expr)
  ;; (cond ...
  (define (c expr)
    (c2 (cdr expr)))
  ;; ((c1 ...)(c2 ...)(else ...))
  (define (c2 expr)
    (let ((condition (car (car expr)))
	  (consequences (cdr (car expr))))
      (cond
       ((eq? condition 'else)
	(cons 'begin consequences))
       (else (list 'if
		   condition
		   (cons 'begin consequences)
		   (c2 (cdr expr)))))))
  (c expr)))



;; let macro
;; (let ((a 1 2 3)(b 2 3 4)(c 5 6 7)) ... )
;; ((lambda (a b c) ...) (begin 1 2 3)(begin 2 3 4)(begin 5 6 7))
(install-macro
 'let
 (lambda (expr)
   (append (list (append (list 'lambda)
			 (list (map car (cadr expr)))
			 (cddr expr)))  
	   (map (lambda (x) (cons 'begin (cdr x))) (cadr expr)))))

;; let*
;; (let* ((a 1 2 3)(b 4 5 6)(c 7 8 9)) ... )
;; ->
;; (let ((a 1 2 3)) (let ((b 4 5 6)) (let ((c 7 8 9)) ...)))
;; ->
;;
(install-macro
 'let*
 (lambda (expr)
   (define (c expr)
     (c2 (car (cdr expr))  (cddr expr)))

   (define (c2 varvals body)
     (cond
      ((null? varvals) (cons 'begin body))
      (else `(let (,(car varvals)) ,(c2 (cdr varvals) body)))))
   (c expr)))

;; EXAMPLE - REWRITE LET* IN TERMS OF LET
;; EXAMPLE - LET IN TERMS OF LAMBDA 

;; letrec
(install-macro
 'letrec
 (lambda (expr)
   (define (c expr)
     (c2 (car (cdr expr))  (cddr expr)))
   (define (c2 varvals body)
     (cond
      ((null? varvals) (cons 'begin body))
      (else (append
	     (list 'let)
	     (list (map (lambda (x) (list (car x) #f)) varvals))
	     (list (append (list 'begin)
			   (append
			    (map (lambda (x) (list 'set! (car x) (cons 'begin (cdr x)))) varvals)
			    body)))))))
   (c expr)))

;;
;; case
;;

;; ----------------


(macro-expand '(when 1 (when 2 `(1 2 3) 3 4 5 "job done")))
(macro-expand '(cond (1 2 3)(4 5 6)(7 8 9)(else (when 1 (when 2 `(1 2 3) 3 4 5 "job done")))))
(macro-expand '(let ((a 1 2 3)(b 4 5 6)(c 7 8 9)) (list a b c)(list c b a)(list b a c)))
(macro-expand '(let* ((a 1 2 3)(b 4 5 6)(c 7 8 9)) (list a b c)(list c b a)(list b a c)))

(pp (macro-expand '(letrec ((even?
			 (lambda (n)
			   (if (zero? n)
			       #t
			       (odd? (- n 1)))))
			(odd?
			 (lambda (n)
			   (if (zero? n)
			       #f
			       (even? (- n 1))))))
		     (even? 88))))

(pp (macro-expand '(letrec ((fac (lambda (n) (if (< n 2) n (* n (fac (- n 1))))))) (list (fac 5)(fac 10)))))


































  
  


  
