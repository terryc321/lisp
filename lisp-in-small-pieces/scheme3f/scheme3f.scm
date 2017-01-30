;;; $Id: chap3f.scm,v 4.0 1995/07/10 06:51:14 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))

;;
;; to run the interpreter
;;  (scheme3f)
;;
;; to run the test suite
;;  (test-scheme3f "scheme.tst")
;;
;; This file requires 2 more files
;;   tester.scm  - actual test suite mechanics
;;   scheme.tst  - a series of scheme question and answers
;;
;; limitations
;; 


(load "tester.scm")

(define wrong error)

;;****************** C L A S S E S ********************

;; tagged types
(define (tag t obj) (cons t obj))
(define (untag x)  (cdr x))
(define (type-of obj)  (car obj))
(define (guard t obj)
  (if (and (pair? obj)
	   (eq? t (car obj)))
      #t
      (wrong "guard stopped execution" t obj)))

;; ***************************************************************
(define (make-primitive name2 address2)
  (tag 'primitive `((name . ,name2)(address . ,address2))))

(define (primitive? x)
  (eq? (type-of x) 'primitive))

(define (primitive-name x)
  (guard 'primitive x)
  (cdr (assoc 'name (untag x))))

(define (primitive-address x)
  (guard 'primitive x)
  (cdr (assoc 'address (untag x))))


  
;; ***************************************************************
(define (make-function vars2 body2 env2)
  (tag 'function `((vars . ,vars2)(body . ,body2)(env  . ,env2))))

(define (function? x)
  (eq? (type-of x) 'function))

(define (function-vars x)
  (guard 'function x)
  (cdr (assoc 'vars (untag x))))

(define (function-body x)
  (guard 'function x)
  (cdr (assoc 'body (untag x))))

(define (function-env x)
  (guard 'function x)
  (cdr (assoc 'env (untag x))))


;; ***************************************************************
(define (make-env)
  (tag 'env `()))

(define (env? x) 
  (eq? (type-of x) 'env))

;; ***************************************************************
(define (make-null-env)
  (tag 'null-env `()))

(define (null-env? x) 
  (eq? (type-of x) 'null-env))

;; ***************************************************************
(define (make-full-env others name)
  (tag 'full-env `((others . ,others)(name . ,name))))

(define (full-env? x)
  (eq? (type-of x) 'full-env))

(define (full-env-others x)
  (guard 'full-env x)
  (cdr (assoc 'others (untag x))))

(define (full-env-name x)
  (guard 'full-env x)
  (cdr (assoc 'name (untag x))))


;; ***************************************************************
(define (make-var-env others name value)
  (tag 'var-env `((others . ,others)(name . ,name)(value  . ,value))))

(define (var-env? x)
  (eq? (type-of x) 'var-env))

(define (var-env-others x)
  (guard 'var-env x)
  (cdr (assoc 'others (untag x))))

(define (var-env-name x)
  (guard 'var-env x)
  (cdr (assoc 'name (untag x))))

(define (var-env-value x)
  (guard 'var-env x)
  (cdr (assoc 'value (untag x))))

(define (var-env-value-set! x v)
  (guard 'var-env x)
  (set-cdr! (assoc 'value (untag x)) v))


;; ***************************************************************
(define (make-cont k) #f)

;; ***************************************************************
(define (make-bottom-cont k f)
  (tag 'bottom-cont `((k . ,k)(f . ,f))))

(define (bottom-cont? x)
  (eq? (type-of x) 'bottom-cont))

(define (bottom-cont-k x)
  (guard 'bottom-cont x)
  (cdr (assoc 'k (untag x))))

(define (bottom-cont-f x)
  (guard 'bottom-cont x)
  (cdr (assoc 'f (untag x))))


;; ***************************************************************
(define (make-if-cont k et ef r)
  (tag 'if-cont `((k . ,k)(et . ,et)(ef . ,ef)(r . ,r))))

(define (if-cont? x)
  (eq? (type-of x) 'if-cont))

(define (if-cont-k x)
  (guard 'if-cont x)
  (cdr (assoc 'k (untag x))))

(define (if-cont-et x)
  (guard 'if-cont x)
  (cdr (assoc 'et (untag x))))

(define (if-cont-ef x)
  (guard 'if-cont x)
  (cdr (assoc 'ef (untag x))))

(define (if-cont-r x)
  (guard 'if-cont x)
  (cdr (assoc 'r (untag x))))


  
;; ***************************************************************
(define (make-begin-cont k e* r)
  (tag 'begin-cont `((k . ,k)(e* . ,e*)(r . ,r))))

(define (begin-cont? x)
  (eq? (type-of x) 'begin-cont))

(define (begin-cont-k x)
  (guard 'begin-cont x)
  (cdr (assoc 'k (untag x))))

(define (begin-cont-e* x)
  (guard 'begin-cont x)
  (cdr (assoc 'e* (untag x))))

(define (begin-cont-r x)
  (guard 'begin-cont x)
  (cdr (assoc 'r (untag x))))



;; ***************************************************************
(define (make-set-cont k n r)
  (tag 'set-cont `((k . ,k)(n . ,n)(r . ,r))))

(define (set-cont? x)
  (eq? (type-of x) 'set-cont))

(define (set-cont-k x)
  (guard 'set-cont x)
  (cdr (assoc 'k (untag x))))

(define (set-cont-n x)
  (guard 'set-cont x)
  (cdr (assoc 'n (untag x))))

(define (set-cont-r x)
  (guard 'set-cont x)
  (cdr (assoc 'r (untag x))))


;; ***************************************************************
(define (make-evfun-cont k e* r)
  (tag 'evfun-cont `((k . ,k)(e* . ,e*)(r . ,r))))

(define (evfun-cont? x)
  (eq? (type-of x) 'evfun-cont))

(define (evfun-cont-k x)
  (guard 'evfun-cont x)
  (cdr (assoc 'k (untag x))))

(define (evfun-cont-e* x)
  (guard 'evfun-cont x)
  (cdr (assoc 'e* (untag x))))

(define (evfun-cont-r x)
  (guard 'evfun-cont x)
  (cdr (assoc 'r (untag x))))


;; ***************************************************************
(define (make-apply-cont k f r)
  (tag 'apply-cont `((k . ,k)(f . ,f)(r . ,r))))

(define (apply-cont? x)
  (eq? (type-of x) 'apply-cont))

(define (apply-cont-k x)
  (guard 'apply-cont x)
  (cdr (assoc 'k (untag x))))

(define (apply-cont-f x)
  (guard 'apply-cont x)
  (cdr (assoc 'f (untag x))))

(define (apply-cont-r x)
  (guard 'apply-cont x)
  (cdr (assoc 'r (untag x))))

;; ***************************************************************
(define (make-arg-cont k e* r)
  (tag 'arg-cont `((k . ,k)(e* . ,e*)(r . ,r))))

(define (arg-cont? x)
  (eq? (type-of x) 'arg-cont))

(define (arg-cont-k x)
  (guard 'arg-cont x)
  (cdr (assoc 'k (untag x))))

(define (arg-cont-e* x)
  (guard 'arg-cont x)
  (cdr (assoc 'e* (untag x))))

(define (arg-cont-r x)
  (guard 'arg-cont x)
  (cdr (assoc 'r (untag x))))
  

;; ***************************************************************
(define (make-gather-cont k v)
  (tag 'gather-cont `((k . ,k)(v . ,v))))

(define (gather-cont? x)
  (eq? (type-of x) 'gather-cont))

(define (gather-cont-k x)
  (guard 'gather-cont x)
  (cdr (assoc 'k (untag x))))

(define (gather-cont-v x)
  (guard 'gather-cont x)
  (cdr (assoc 'v (untag x))))

;; ***************************************************************

(define (cont? x)
  (or
   (bottom-cont? x)
   (if-cont? x)
   (begin-cont? x)
   (set-cont? x)
   (evfun-cont? x)
   (apply-cont? x)
   (arg-cont? x)   
   (gather-cont? x)))


;; ***************************************************************
;; invoke


(define (invoke f v* r k)
  (cond
   ((primitive? f)
    ((primitive-address f) v* r k))    
   ((cont? f)
      (if (= 1 (length v*))
      (resume f (car v*))
      (wrong "Conts expect one argument" v* r k)))
   ((function? f)
      (let ((env (extend-env (function-env f)
                         (function-vars f)
                         v* )))
	(evaluate-begin (function-body f) env k)))
   (else
    (wrong " <invoke> not a function " f r k))))



;; ***************************************************************
;; resume

(define (resume k v)
  (cond
   ((if-cont? k)
    (evaluate (if v (if-cont-et k) (if-cont-ef k))
	      (if-cont-r k)
	      (if-cont-k k)))
   
   ((begin-cont? k)
    (evaluate-begin (cdr (begin-cont-e* k)) 
		    (begin-cont-r k) 
		    (begin-cont-k k)))
   
   ((set-cont? k)
    (update! (set-cont-r k) (set-cont-n k) (set-cont-k k) v))

   ((evfun-cont? k)
    (evaluate-arguments (evfun-cont-e* k)
			(evfun-cont-r k)
			(make-apply-cont
			 (evfun-cont-k k) ;; k f r
			 v ;; was f
			 (evfun-cont-r k))))
   
   ((arg-cont? k)
    (evaluate-arguments (cdr (arg-cont-e* k)) 
			(arg-cont-r k)
			(make-gather-cont ;; k v
			 (arg-cont-k k)
			 v)))
   
   ((apply-cont? k)
    (invoke (apply-cont-f k) 
	    v
	    (apply-cont-r k)
	    (apply-cont-k k)))

   ((gather-cont? k)
    (resume (gather-cont-k k) (cons (gather-cont-v k) v)))
   
   ((bottom-cont? k)
    ((bottom-cont-f k) v))
   
   (else
    (wrong " <resume> unknown cont " k v))))


;; ***************************************************************
;; lookup

(define (lookup r n k)
  (cond
   ((var-env? r)
    (if (eqv? n (var-env-name r))
	(begin 'found
	       (resume k (var-env-value r)))
	(lookup (var-env-others r) n k)))
   ((full-env? r)
    (lookup (full-env-others r) n k))
   ((null-env? r)
    (wrong "Unknown variable" n r k))
   (else
    (wrong "not an env" r n k))))

;; ***************************************************************
;; update

(define (update! r n k v)
  (cond
   ((var-env? r)
    (if (eqv? n (var-env-name r))
	(begin
	  (var-env-value-set! r v) ;; <-- set slot value	
	  (if (equal? v (var-env-value r))
	      'ok
	      (wrong "update (r <var-env>) n k v :: fails to do its purpose -- Line 271 " r n k v))	
	  ;; verify lookup and set slot are same
	  (lookup r n k) ;; lookup automatically call resume k v
	  ;;(resume k v)
	  )
	(update! (var-env-others r) n k v)))
   
   ((full-env? r)
    (update! (full-env-others r) n k v))
   
   ((null-env? r)
    (wrong "Unknown variable" n r k v))
   
   (else
    (wrong "not an env" r n k v))))



;; ********************** I N T E R P R E T E R ************************
;;; An interpreter with objects from chapter 3.
(define (atom? e) (not (pair? e)))

;; e,et,ec,ef ........ expression , form
;; r ................. env
;; k,kk .............  continuation
;; v ................. value (integer, pair , closure)
;; f ................. function
;; n ................. identifier

(define (evaluate e r k)
  (if (atom? e)
    (cond ((symbol? e) (evaluate-variable e r k))
          (else        (evaluate-quote e r k)))
    (case (car e)
     ((quote)  (evaluate-quote (cadr e) r k))
     ((if)     (evaluate-if (cadr e) (caddr e) (cadddr e) r k))
     ((begin)  (evaluate-begin (cdr e) r k))
     ((set!)   (evaluate-set! (cadr e) (caddr e) r k))
     ((lambda) (evaluate-lambda (cadr e) (cddr e) r k))
     (else     (evaluate-application (car e) (cdr e) r k)))))

(define (evaluate-variable n r k)
  (lookup r n k))

;; evaluate passes quoted value as v , env r , continuation k unchanged
;; then calls resume
(define (evaluate-quote v r k)
  (resume k v))

;; evaluate a conditional expression ec
;; expression true part et
;; expression false part ef
;; env r
;; continuation k , expects the result
;;
;; to evaluate if special form
;; evaluate conditional expression ec in the env r
;; giving it a continuation -
;;
;; compare to sicp version.
;;
(define (evaluate-if ec et ef r k)
  (evaluate ec r (make-if-cont k et ef r)))

(define empty-begin-value '())


(define (evaluate-begin e* r k)
  (if (pair? e*)
    (if (pair? (cdr e*))
      (evaluate (car e*) r (make-begin-cont k e* r))
      (evaluate (car e*) r k) )
    (resume k empty-begin-value)))

(define (evaluate-set! n e r k)
  (evaluate e r (make-set-cont k n r)))


(define (evaluate-lambda n* e* r k)
  (resume k (make-function n* e* r)))

(define (extend-env env names values)
  (cond ((and (pair? names) (pair? values))      
         (make-var-env
	  (extend-env env (cdr names) (cdr values))
          (car names)
          (car values)))
        ((and (null? names) (null? values)) env)
        ((symbol? names) (make-var-env
			  env
			  names
			  values))
        (else (wrong "Arity mismatch"))))


(define (evaluate-application e e* r k)
  (evaluate e r (make-evfun-cont k e* r)))

(define (evaluate-arguments e* r k)
  (if (pair? e*)
    (evaluate (car e*) r (make-arg-cont k e* r))
    (resume k no-more-arguments)))

(define no-more-arguments '())

(define (chapter3-interpreter)
  (define (toplevel)
    (evaluate (read) 
              r.init 
              (make-bottom-cont 'void display))
    (toplevel))
  (toplevel))


;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initial env

(define r.init (make-null-env))

(define-syntax definitial 
  (syntax-rules ()
    ((definitial name)
     (definitial name 'void))
    ((definitial name value)
     (begin (set! r.init (make-var-env
			  r.init
			  'name
			  value))
            'name))))


(define-syntax defprimitive 
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name 
       (make-primitive
	 'name
	 (lambda (v* r k) 
	   (if (= arity (length v*))
	       (resume k (apply value v*))
	       (wrong "Incorrect arity" 'name v*))))))))

(define-syntax defpredicate 
  (syntax-rules ()
    ((defpredicate name value arity)
     (defprimitive name
       (lambda values (apply value values))
       arity))))

(definitial t #t)
(definitial f #f)
(definitial nil '())

(definitial x)
(definitial y)
(definitial z)
(definitial a)
(definitial b)
(definitial c)
(definitial foo)
(definitial bar)
(definitial hux)
(definitial fib)
(definitial fact)
(definitial visit)
(definitial length)
(definitial primes)

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defpredicate pair? pair? 1)
(defpredicate symbol? symbol? 1)
(defprimitive eq? eq? 2)
(defpredicate eq? eq? 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defpredicate = = 2)
(defprimitive < < 2)
(defpredicate < < 2)
(defpredicate > > 2)
(defprimitive * * 2)
(defpredicate <= <= 2)
(defpredicate >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive display display 1)

(definitial call/cc 
  (make-primitive
    'call/cc
    (lambda (v* r k) 
      (if (= 1 (length v*))
	  (invoke (car v*) (list k) r k)
	  (wrong "Incorrect arity" 'call/cc v*)))))


(definitial apply
  (make-primitive
   'apply
   (lambda (v* r k)
     (if (>= (length v*) 2)
	 (let ((f (car v*))
	       (args (let flat ((args (cdr v*)))
		       (if (null? (cdr args))
			   (car args)
			   (cons (car args) (flat (cdr args)))))))
	   (invoke f args r k))
	 (wrong "Incorrect arity" 'apply)))))


(definitial list 
  (make-primitive
   'list
   (lambda (v* r k) (resume k v*))))



;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;;  Tests

(define (scheme3f)
  (interpreter 
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read print error)
     (define k.init (make-bottom-cont 'anything print))
     (set! wrong error)
     (lambda ()
       (print (evaluate (read) r.init k.init))))))

(define (test-scheme3f file)
  (suite-test
   file
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read check error)
     (define k.init (make-bottom-cont 'void check))
     (set! wrong error)
     (lambda ()
       (check (evaluate (read) r.init k.init))))
   equal?))

;;; end of chap3f.scm

















