

;;; syntactic closures


;; (push a l) ==> (set! l (cons a l))

(define push-transformer
  (lambda (form env)
    (let ((a-form (make-syntactic-closure env '() (cadr form)))
	  (l-form (make-syntactic-closure env '() (caddr form))))
      (make-syntactic-closure
       *global-syntactic-environment*
       '()
       `(set! ,l-form (cons ,a-form ,l-form))))))


(define or-transformer
  (lambda (form env)
    (let ((operands (map (lambda (operand)
			   (make-syntactic-closure env '() operand))
			 (cdr form))))
      (cond
       ((null? operands)
	(make-syntactic-closure env '() '#f))
       ((null? (cdr operands))
	(car operands))
       (else
	(make-syntactic-closure
	 *global-syntactic-environment*
	 '()
	 `((lambda (temp)
	     (if temp
		 temp
		 (or ,@(cdr operands))))
	   ,(car operands))))))))


(define let-transformer
  (lambda (form env)
    (let* ((vars
	    (map car (cadr form)))
	   (init-forms
	    (map (lambda (spec)
		   (make-syntactic-closure env '() (cadr spec)))
		 (cadr form)))
	   (body-forms
	    (map (lambda (body-form)
		   (make-syntactic-closure env vars body-form))
		 (cddr form))))
      (make-syntactic-closure
       *global-syntactic-environment*
       '()
       `((lambda ,vars
	   ,@body-forms)
	 ,@init-forms)))))



(define catch-transformer
  (lambda (form env)
    (let ((body-form
	   (make-syntactic-closure env '(throw) (cadr form))))
      (make-syntactic-closure
       *global-syntactic-environment*
       '()
       `(call-with-current-continuation
	 (lambda (throw)
	   ,body-form))))))


;; the syntactic closures expander

(define synclo-expand
  (lambda (exp)
    (synclo-expand-aux exp *global-syntactic-environment*)))

(define synclo-expand-aux
  (lambda (exp synenv)
    (cond
     ((symbol? exp)
      (expand-symbol exp synenv))
     ((syntactic-closure? exp)
      (expand-syntactic-closure exp synenv))
     ((pair? exp)
      (expand-pair exp synenv))
     (else exp))))


(define expand-symbol
  (lambda (exp synenv)
    (let ((meaning (lookup exp synenv)))
      (cond
       ((symbol? meaning) meaning)
       ((free? meaning) exp)
       (else (error "symbol doesn't denote a variable" exp))))))




(define expand-syntactic-closure
  (lambda (exp synenv)
    (synclo-expand-aux (syntactic-closure-form exp)
		       (combine-syntactic-environments
			(syntactic-closure-free-names exp)
			synenv
			(syntactic-closure-environment exp)))))


(define expand-pair
  (lambda (exp synenv)
    (if (not (symbol? (car exp)))
	(map (lambda (exp) (synclo-expand-aux exp synenv)) exp)
	(let ((meaning (lookup (car exp) synenv)))
	  (cond
	   ((procedure? meaning)
	    (synclo-expand-aux (meaning exp synenv)
			       *bogus-syntactic-environment*))
	   ((core? meaning)
	    (expand-core exp synenv))
	   (else
	    (map (lambda (exp) (synclo-expand-aux exp synenv))
		 exp)))))))

(define expand-core
  (lambda (exp synenv)
    (case (car exp)
      ((quote) exp)
      ((set! if)
       (cons (car exp)
	     (map (lambda (exp) (synclo-expand-aux exp synenv))
		  (cdr exp))))
      ((lambda)
       (let* ((new-syms (map symbol->unique-symbol (cadr exp)))
	      (new-env (extend-syntactic-environment
			synenv
			(cadr exp)
			new-syms)))
	 `(lambda ,new-syms
	    ,@(map (lambda (exp)
		     (synclo-expand-aux exp new-env))
		   (cddr exp))))))))



(define combine-syntactic-environments
  (lambda (names names-synenv else-synenv)
    (let ((meanings (map (lambda (name) (lookup name names-synenv))
			 names)))
      (extend-syntactic-environment else-synenv
				    names
				    meanings))))

(define extend-syntactic-environment
  (lambda (synenv names meanings)
    (cons (cons names meanings)
	  synenv)))

(define lookup
  (lambda (name env)
    (cond
     ((bogus-environment? env)
      (error "macro transformer didn't enclose form"))
     ((null? env)
      #f)			; #f means `free'
     (else
      (search-frame
       name
       (caar env)
       (cdar env)
       (lambda ()
	 (lookup name (cdr env))))))))

(define search-frame
  (lambda (name frame-names frame-vals failure-thunk)
    (cond
     ((null? frame-names)
      (failure-thunk))
     ((eq? name (car frame-names))
      (car frame-vals))
     (else
      (search-frame
       name
       (cdr frame-names)
       (cdr frame-vals)
       failure-thunk)))))

(define *core-syntactic-environment*
  '(((quote lambda set! if) . (CORE CORE CORE CORE))))

(define *global-syntactic-environment* *core-syntactic-environment*)

(define *bogus-syntactic-environment* #f)

(define bogus-environment?
  (lambda (x)
    (eq? x *bogus-syntactic-environment*)))

(define free?
  (lambda (x)
    (eq? x #f)))

(define core?
  (lambda (x)
    (eq? x 'CORE)))



(define make-syntactic-closure
  (lambda (synenv free-names form)
    (vector 'SYNTACTIC-CLOSURE synenv free-names form)))

(define syntactic-closure?
  (lambda (x)
    (and (vector? x)
	 (= (vector-length x) 4)
	 (eq? (vector-ref x 0) 'SYNTACTIC-CLOSURE))))

(define syntactic-closure-environment
  (lambda (x)
    (vector-ref x 1)))

(define syntactic-closure-free-names
  (lambda (x)
    (vector-ref x 2)))

(define syntactic-closure-form
  (lambda (x)
    (vector-ref x 3)))

(define symbol->unique-symbol
  (lambda (sym)
    (gensym)))



;; add macros onto the syntactic environment
(define (install-transformer sym transformer)
  (set! *global-syntactic-environment*
    (extend-syntactic-environment
     *global-syntactic-environment*
     (list sym)
     (list transformer))))




(install-transformer 'push push-transformer)
(install-transformer 'or or-transformer)
(install-transformer 'let let-transformer)



(define demo0 (synclo-expand
	       '(let ((temp 37.0))
		  (or (foo temp)
		      temp))))

(define demo1 (synclo-expand
	       '(let ((cons '()))
		  (push "ghengis" cons)
		  (push "khubla" cons)
		  cons)))

;;----------- EXPERIMENTAL --------------------------------
;; ;; here RESERVED word IF , doesnt like being used as variable !!
;; (define demo2 (synclo-expand
;; 	       '(let ((if 3))
;; 		  (if if if 'something-else))))

;; ;; is SET! 
;; (define demo3 (synclo-expand
;; 	       '(let ((set! 3))
;; 		  (if set! set! 'something-else))))

;; ;; attempt to alter primitive set! to our own purposes
;; (let ((old-set! set!))
;;   (define set! (lambda (a b)
;; 		 (display "setting symbol ")
;; 		 (display a)
;; 		 (newline)
;; 		 (old-set! a b))))


;; okay lets try our own
;; (swap a b)
;; (let ((temp a))
;;   (set! a b)
;;   (set! b temp))

(define swap-transformer
  (lambda (form env)
    (let ((a (make-syntactic-closure env '() (car (cdr form))))
	  (b (make-syntactic-closure env '() (car (cdr (cdr form))))))
      (make-syntactic-closure
       *global-syntactic-environment*
       '()
       `(let ((temp ,a))
	  (set! ,a ,b)
	  (set! ,b temp))))))





;; (define swap-transformer
;;   (lambda (form env)
;;     (let ((operands (map (lambda (operand)
;; 			   (make-syntactic-closure env '() operand))
;; 			 (cdr form))))
;;       (make-syntactic-closure
;;        *global-syntactic-environment*
;;        '()
;;        `((lambda (temp)
;; 	   (set! temp ,(car operands))
;; 	   (set! ,(car operands) ,(car (cdr operands)))
;; 	   (set! ,(car (cdr operands)) temp))
;; 	 ,(car operands))))))))

;; now install this transformer.
(install-transformer 'swap swap-transformer)

(define demo-swap (synclo-expand
		   '(let ((a 5)(b 10))
		      (list a b)
		      (swap a b)
		      (list a b))))


(define demo-swap2 (synclo-expand
		   '(let ((a 5)(b 10))
		      (list a b)
		      (swap a b)
		      (swap a b)
		      (list a b))))


(define demo-swap3 (synclo-expand
		   '(let ((a 5)(b 10))
		      (list a b)
		      (swap a b)
		      (swap a b)
		      (swap a b)		  		      
		      (list a b))))




