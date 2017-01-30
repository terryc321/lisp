;;; $Id: chap3f.scm,v 4.0 1995/07/10 06:51:14 queinnec Exp $

;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))
;;; This file is part of the files that accompany the book:
;;;     LISP Implantation Semantique Programmation (InterEditions, France)
;;; By Christian Queinnec <Christian.Queinnec@INRIA.fr>
;;; Newest version may be retrieved from:
;;;   (IP 128.93.2.54) ftp.inria.fr:INRIA/Projects/icsla/Books/LiSP*.tar.gz
;;; Check the README file before using this file.
;;;(((((((((((((((((((((((((((((((( L i S P ))))))))))))))))))))))))))))))))


;; e,et,ec,ef ........ expression , form
;; r ................. environment
;; k,kk .............  continuation
;; v ................. value (integer, pair , closure)
;; f ................. function
;; n ................. identifier


;; guile specific stuff 
(use-modules (ice-9 format))  ;; format = nice print to strings and ports
(use-modules (oop goops)) ;; **modified ** object orientated stuff for overkill perhaps

;; wrong ?

;; goops inheritance
(define-class <2d-vector> ()
  (x #:init-value 0 #:accessor x-component #:init-keyword #:x)
  (y #:init-value 0 #:accessor y-component #:init-keyword #:y))

(define-class <3d-vector> (<2d-vector>)
  (z #:init-value 0 #:accessor z-component #:init-keyword #:z))

(define v (make <3d-vector> #:x 1 #:y 2 #:z 3))

;;; An interpreter with objects from chapter 3.
(define (atom? e) (not (pair? e)))

;; e expression
;; r environment
;; k continuation
(define (evaluate e r k)
  (if (atom? e)
    (cond ((symbol? e) (evaluate-variable e r k))
          (else        (evaluate-quote e r k)) )
    (case (car e)
     ((quote)  (evaluate-quote (cadr e) r k))
     ((if)     (evaluate-if (cadr e) (caddr e) (cadddr e) r k))
     ((begin)  (evaluate-begin (cdr e) r k))
     ((set!)   (evaluate-set! (cadr e) (caddr e) r k))
     ((lambda) (evaluate-lambda (cadr e) (cddr e) r k))
     (else     (evaluate-application (car e) (cdr e) r k)))))

(define (evaluate-variable n r k)
  (lookup r n k)) 



;;************* VALUES 
(define-class <value> ())

(define-class <primitive> (<value>)
  (name #:init-value 0 #:accessor primitive-name #:init-keyword #:name)
  (address #:init-value 0 #:accessor primitive-address #:init-keyword #:address))



(define-class <environment> ())

(define-class <null-env> (<environment>))

(define-class <full-env> (<environment>)
  (others #:init-value #f #:accessor full-env-others #:init-keyword #:others)
  (name #:init-value #f #:accessor full-env-name #:init-keyword #:name))

(define-class <var-env> (<full-env>)
  (value #:init-value #f #:accessor var-env-value #:init-keyword #:value))


;;*********** CONTINUATIONS 
(define-class <continuation> ()
  (k #:init-value #f #:accessor cont-k #:init-keyword #:k))

(define-class <bottom-cont> (<continuation>)
  (f #:init-value #f #:accessor bottom-cont-f #:init-keyword #:f))

(define-class <if-cont> (<continuation>)
  (et #:init-value #f #:accessor if-cont-et #:init-keyword #:et)
  (ef #:init-value #f #:accessor if-cont-ef #:init-keyword #:ef)
  (r #:init-value #f #:accessor if-cont-r #:init-keyword #:r))


;;**************** I N V O K E ***********************
(define-generic invoke)
(define-method (invoke f v* r k)
  (wrong "not a function" f r k)) 
(define-method (invoke (f <primitive>) v* r k)
  ((primitive-address f) v* r k) )
(define-method (invoke (f <continuation>) v* r k)
  (if (= 1 (length v*))
      (resume f (car v*))
      (wrong "Continuations expect one argument" v* r k)))
(define-method (invoke (f <function>) v* r k)
  (let ((env (extend-env (fun-env f)
                         (fun-vars f)
                         v* )))
    (evaluate-begin (fun-body f) env k)))


;; **************** R E S U M E *********************
(define-generic resume)
(define-method (resume (k <continuation>) v)
  (wrong "Unknown continuation" k))
(define-method (resume (k <if-cont>) v)
  (evaluate (if v (if-cont-et k) (if-cont-ef k))
	    (if-cont-r k)
	    (cont-k k)))
(define-method (resume (k <begin-cont>) v)
  (evaluate-begin (cdr (begin-cont-e* k)) 
                  (begin-cont-r k) 
                  (cont-k k)))
(define-method (resume (k <set!-cont>) v)
  (update! (set!-cont-r k) (set!-cont-n k) (cont-k k) v))
(define-method (resume (k evfun-cont) f)
  (evaluate-arguments (evfun-cont-e* k)
                      (evfun-cont-r k)
                      (make <apply-cont>
			#:k (evfun-cont-k k)
			#:f f
			#:r (evfun-cont-r k))))
(define-method (resume (k <argument-cont>) v)
  (evaluate-arguments (cdr (arg-cont-e* k)) 
                      (arg-cont-r k)
                      (make <gather-cont>
			#:k (arg-cont-k k)
			#:v v)))

(define-method (resume (k <gather-cont>) v*)
  (resume (gather-cont-k k) (cons (gather-cont-v k) v*)) )
(define-method (resume (k <apply-cont>) v)
  (invoke (apply-cont-f k) 
          v
          (apply-cont-r k)
          (apply-cont-k k)))
(define-method (resume (k <bottom-cont>) v)
  ((bottom-cont-f k) v))

;; ************************ L O O K U P *****************
(define-generic lookup)
(define-method (lookup (r <environment>) n k)
  (wrong "not an environment" r n k) )
(define-method (lookup (r <null-env>) n k)
  (wrong "Unknown variable" n r k) )
(define-method (lookup (r <full-env>) n k)
  (lookup (full-env-others r) n k))
(define-method (lookup (r <var-env>) n k)
  (if (eqv? n (var-env-name r))
    (resume k (var-env-value r))
    (lookup (var-env-others r) n k) ) )

;; *********************** U P D A T E *****************
(define-generic update!)
(define-method (update! (r <environment>) n k v)
  (wrong "not an environment" r n k) )


(define (evaluate-quote v r k)
  (resume k v) )

(define (evaluate-if ec et ef r k)
  (evaluate ec r (make <if-cont>
		   #:k k 
                   #:et et
		   #:ef ef
		   #:r r)))


(define-class <begin-cont> (<continuation>)
  (e* #:init-value #f #:accessor begin-cont-e* #:init-keyword #:et)
  (r #:init-value #f #:accessor begin-cont-r #:init-keyword #:r))

(define empty-begin-value #f)

(define (evaluate-begin e* r k)
  (if (pair? e*)
    (if (pair? (cdr e*))
      (evaluate (car e*) r (make <begin-cont> #:k k #:e* e* #:r r))
      (evaluate (car e*) r k) )
    (resume k empty-begin-value)))


(define-class <set!-cont> (<cont>)
  (n #:init-value #f #:accessor set!-cont-n #:init-keyword #:n)
  (r #:init-value #f #:accessor set!-cont-r #:init-keyword #:r))


(define (evaluate-set! n e r k)
  (evaluate e r (make <set!-cont> #:k k #:n n #:r r)) )


(define-method (update! (r <null-env>) n k v)
  (wrong "Unknown variable" n r k) )

(define-method (update! (r <full-env>) n k v)
  (update! (full-env-others r) n k v) )

 ;; *** set slot ***slot-set! obj slot-name value
(define (var-env-name r) (full-env-name r))

(define (var-env-value! r v) (slot-set! r #:value v))

(define-method (update! (r <var-env>) n k v)
  (if (eqv? n (var-env-name r))
    (begin (set-var-env-value! r v) ;; <-- set slot value
           (resume k v))
    (update! (var-env-others r) n k v) ) )

(define-class <fun> (<value>)
  (vars #:init-value #f #:accessor fun-vars #:init-keyword #:vars)
  (body #:init-value #f #:accessor fun-body #:init-keyword #:body)
  (env #:init-value #f #:accessor fun-env #:init-keyword #:env))


(define (evaluate-lambda n* e* r k)
  (resume k (make <fun> #:vars n* #:body e* #:env r)))


(define (extend-env env names values)
  (cond ((and (pair? names) (pair? values))      
         (make <var-env>
          #:others (extend-env env (cdr names) (cdr values))
          #:name (car names)
          #:value (car values)))
        ((and (null? names) (null? values)) env)
        ((symbol? names) (make <var-env> #:others env #:name names #:values values))
        (else (wrong "Arity mismatch"))))

(define-class <evfun-cont> (<continuation>)
  (e* #:init-value #f #:accessor evfun-cont-e #:init-keyword #:e*)
  (r #:init-value #f #:accessor evfun-cont-r #:init-keyword #:r))

(define-class <apply-cont> (<continuation>)
  (f #:init-value #f #:accessor apply-cont-f #:init-keyword #:f)
  (r #:init-value #f #:accessor apply-cont-r #:init-keyword #:r))

(define-class <argument-cont> (<continuation>)
  (e* #:init-value #f #:accessor arg-cont-e #:init-keyword #:e*)
  (r #:init-value #f #:accessor arg-cont-r #:init-keyword #:r))

(define-class <gather-cont> (<continuation>)
  (v #:init-value #f #:accessor gather-cont-v #:init-keyword #:v))

(define (evaluate-application e e* r k)
  (evaluate e r (make <evfun-cont>
		  #:k k
		  #:e* e*
		  #:r r)))


(define (evaluate-arguments e* r k)
  (if (pair? e*)
    (evaluate (car e*) r (make-argument-cont k e* r))
    (resume k no-more-arguments)))

(define no-more-arguments '())




(define (chapter3-interpreter)
  (define (toplevel)
    (evaluate (read) 
              r.init 
              (make <bottom-cont> #:k 'void #:f display))
    (toplevel))
  (toplevel))

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initial environment

(define r.init (make-null-env))

(define-syntax definitial 
  (syntax-rules ()
    ((definitial name)
     (definitial name 'void) )
    ((definitial name value)
     (begin (set! r.init (make-variable-env r.init 'name value))
            'name ) ) ) )

(define-syntax defprimitive 
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name 
       (make-primitive 
        'name (lambda (v* r k) 
                (if (= arity (length v*))
                    (resume k (apply value v*))
                    (wrong "Incorrect arity" 'name v*) ) ) ) ) ) ) )

(define-syntax defpredicate 
  (syntax-rules ()
    ((defpredicate name value arity)
     (defprimitive name
       (lambda values (apply value values))
       arity ) ) ) )

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
         (wrong "Incorrect arity" 'call/cc v*) ) ) ) )

(definitial apply
  (make-primitive 
   'apply
   (lambda (v* r k)
     (if (>= (length v*) 2)
         (let ((f (car v*))
               (args (let flat ((args (cdr v*)))
                       (if (null? (cdr args))
                           (car args)
                           (cons (car args) (flat (cdr args))) ) )) )
           (invoke f args r k) )
         (wrong "Incorrect arity" 'apply) ) ) ) )
             
(definitial list 
  (make-primitive
   'list
   (lambda (v* r k) (resume k v*)) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;;  Tests

(define (scheme3f)
  (interpreter
   "Scheme? "
   "Scheme= "
   #t
   (lambda (read print error)
     (define k.init (make-bottom-cont 'void print))
     (set! wrong error)
     (lambda ()
       (print (evaluate (read) r.init k.init)) ) ) ) )

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
       (check (evaluate (read) r.init k.init)) ) )
   equal? ) )

;;; end of chap3f.scm
