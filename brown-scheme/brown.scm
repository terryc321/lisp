#lang racket


(define-syntax curry
  (syntax-rules () 
    [(curry (i) b ...) (lambda (i) b ...)]
    [(curry (i j ...) b ...)
     (lambda (i) (curry (j ...) b ...))]))


;; (curry (x y z) ...)
;; =>  (lambda (x) (lambda (y) (lambda (z) ...)))
(define-syntax C
  (syntax-rules ()
    [(C m n) (m n)]
    [(C m n p ...) (C (m n) p ... )]))

;; heres what this does ....
;;> (C 'a 'b 'c 'd 'e)
;;(((('a 'b) 'c) 'd) 'e)
;; puts first two together ,
;; anything else becomes added to outside list as 2nd element


(define tower
  (letrec
      ([loop
        (lambda (n)
          (lambda (theta)
            ((theta (R-E-P n)) (loop (add1 n)))))])
    (loop 0)))


(define shift-up
  (lambda (theta)
    (lambda (mk)
      (mk theta))))


(define shift-down
  (lambda (d)
    (lambda (k)
      (lambda (mk)
        (d ((meta-cons k) mk))))))


;; --------- meta continuations -----------
(define meta-cons
  (lambda (k)
    (lambda (mk)
      (lambda (theta)
        ((theta k) mk)))))


;; shorthand version of meta cons
;; (define meta-cons
;;   (curry (k mk theta)
;;          (C theta k mk)))

(define meta-car
  (shift-up
   (lambda (v)
     (lambda (mk)
       v))))

(define meta-cdr
  (shift-up
   (lambda (v)
     (lambda (mk)
       mk))))

;; ;;By expanding these definitions, we may deduce
;; (meta-car ((meta-cons k) mk)) = k
;; (meta-cdr ((meta-cons k) mk)) = mk
;; Conversely, we could have defined the appropriate functions to mk:
;; meta-car and meta-cdr by explicitly passing

;; (define meta-car
;;   (lambda (mk)
;;     (mk (lambda (k) (lambda (mk) k)))))

;; (define meta-cdr
;;   (lambda (mk)
;;     (mk (lambda (k) (lambda (mk) mk)))))

;; ;; Then we could define shift-up in terms of meta-car and meta-cdr
;; (define shift-up
;;   (lambda (th)
;;     (lambda (mk)
;;       ((th (meta-car mk)) (meta-cdr mk)))))


;; Let us imagine that we want to
;; terminate the lower interpreter with value v. To do this, we must pass v to the con-
;; tinuation x waiting in the upper interpreter. Thus we must pass to the metacon-
;; tinuation a thunk that given x, passes v to it. This can be done by invoking a con-
;; tinuation terminate-level defined as follows:

(define terminate-level
  (lambda (v)
    (shift-up (lambda (k) (k v)))))


(define ef
  (lambda (bool x y)
    (if bool x y)))


(define denotation
  (lambda (e)
    (cond
     [(atom? e) (denotation-of-identifier e)]
     [(eq? (first e) 'lambda) (denotation-of-abstraction e)]
     [else (denotation-of-application e)])))


(define denotation-of-identifier
  (curry (e r k)
         (C r e
            (lambda (cell)
              (let ([v (deref-cell cell)])
                (if (eq? v 'UNASSIGNED)
                    (wrong (list "Brown: unbound id " e))
                    (k v)))))))


;; To accomodate reification, Brown uses call-by-text. A Brown function has
;; functionality

(define denotation-of-application
  (curry (e r k)
         (C denotation (first e) r
            (lambda (f) (C f (rest e) r k)))))



(define denotation-of-abstraction
  (curry (e r k)
         (k (F->BF
             (lambda (v*)
               (C denotation (third e)
                  (extend r (second e) v*)))))))


;; The function F->BF takes an element of F (= V-~ K -3 MK A) and turns it into
;; a Brown function that evaluates its actual parameters in the call-time environment
;; and passes the list of results to the function:
;; wot ?
(define F->BF
  (curry (fun e r k)
         (C Y (curry (eval-args e k)
                     (if (null? e) (k '())
                         (C denotation (first e) r
                            (lambda (v)
                              (C eval-args (rest e)
                                 (lambda (w)
                                   (k (cons v w))))))))
            e (curry (v* mk) (C fun v* k mk)))))



;; We next turn to the reifying functions. These functions take objects from the un-
;; derlying domains K and Env, and turn them into Brown functions that can be
;; manipulated [4]. An environment is turned into a one-argument Brown function
;; that evaluates its argument and passes the result (the evaluated actual parameter)
;; to the environment:
(define U->BF
  (curry (rl e r k)
         (if (= (length e) 1)
             (C denotation (first e) r
                (lambda (var) (C rl var k)))
             (wrong (list
                     "U->BF: wrong number of args"
                     e)))))



(define K->BF
  (curry (kl e r k)
         (if (= (length e) 1)
             (C denotation (first e) r
                (lambda (v) (C shift-down (ki v) k)))
             (wrong (list
                     "K->BF: wrong number of args"
                     e)))))


(define make-reifier
  (curry (bf e r k)
         (shift-up (bf (list e
                             (U->BF
                              r)
                             (K->BF k))))))

;; we immediately redefine make-reifier ?
(define make-reifier
  (let
      ([ERK '(E R K)])
    (curry (bf e r k)
           (shift-up (C bf
                        ERK
                        (extend r
                                ERK
                                (list e
                                      (U->BF
                                       r)
                                      (K->BF k))))))))



(define BF->K
  (let ([z '(v)])
    (curry (bf v mk)
           (C bf z (extend global-env z (list v))
              (meta-car mk)
              (meta-cdr mk)))))



(define BF->U
  (let ([z ' (v)])
    (curry (bf v)
           (C bf z
              (extend global-env z (list v))))))


(define meaning
  (curry (erk)
         (shift-down
          (C denotation
             (first erk)
             (BF->U (second erk))
             (BF->K (third erk))))))


(define R-E-P
  (lambda (prompt)
    (Y (curry (loop v)
              (C denotation
                 (prompt&read
                  (print&prompt prompt v))
                 global-env
                 loop)))))


(define tower
  ((Y (curry (loop n theta)
             (C theta (R-E-P n) (loop (add 1 n)))))
   0))

(define readloop
  (lambda (prompt)
    (K->BF (R-E-P prompt))))


(define boot-tower
  (lambda ()
    (C terminate-level 'starting-up tower)))


(define extend
  (lambda (r names vals)
    (if (= (length names) (length vals))
        (let ([cells (map make-cell vals)])
          (curry (name k)
                 (rib-lookup name names cells k
                             (lambda () (C r name k)))))
        (curry (name k)
               (wrong (list "extend:"
                            "Formals: " names
                            "Actuals: " vals))))))
(define rib-lookup
  (lambda (id names cells sk fk)
    (C Y (curry (lookup names cells)
                (cond
                 [(null? names) (fk)]
                 [(eq? (first names) id) (sk (first cells))]
                 [else (C lookup (rest names) (rest cells))]))
       names cells)))


(define id->BF
  (let ([host->F
         (curry (f v* k) (k (apply f v*)))])
    (lambda (x)
      (F->BF (host->F (host-value x))))))


(define global-env #f)

(define boot-global-env
  (let ([id->F-cell (lambda (x) (make-cell (id->BF x)))])
    (lambda ()
      (let ([initnames
             (append
              (list 'nil 't 'wrong 'meaning)
              primop-name-table)]
            [initcells
             (append
              (map make-cell
                   (list nil t
                         (K->BF terminate-level)
                         (F->BF meaning)))
              (map id->F-cell
                   primop-name-table))])
        ;; changed define to set!
        (set! global-env
          (curry (id k)
                 (rib-lookup
                  id initnames initcells k
                  (lambda ()
                    (let
                        ([c (make-cell 'UNASSIGNED)])
                      (set! initnames (cons id initnames))
                      (set! initcells (cons c initcells))
                      (k c))))))))))



(define wrong
  (curry (v mk)
         (writeln "wrong: " v)
         (C terminate-level 'wrong mk)))


;; ------------- helper functions  -------------------



(define first car)
(define second cadr)
(define third caddr)
(define rest cdr)


(define Y
  (lambda (f)
    (let ([d (lambda (x)
               (f (lambda (arg)
                    (C x x arg))))])
      (d d))))


(define prompt&read
  (lambda (prompt)
    (print prompt) (print "-> ") (read)))
(define print&prompt
  (lambda (prompt v)
    (writeln prompt "::" v) prompt))


(define host-value
  (lambda (id) (eval id)))

(define primop-name-table
  (list 'car 'cdr 'cons 'eq? 'atom? 'symbol?
        'null? 'not 'addl 'subl 'zero?
        '+ '– '*
        'set-car! 'set-cdr!
        'print 'length 'read 'newline 'reset
        'make-cell 'deref-cell 'set-cell!
        'ef 'readloop 'make-reifier
        'brown-load))



(define prompt&read
  (curry (prompt k)
         (display prompt) (display "-*") (k (read))))

(define R-E-P
  (lambda (prompt)
    (Y (curry (loop v)
              ((prompt&read (print&prompt prompt v))
               (lambda (v)
                 (if (eof-object? v)
                     (lambda (mk) '---done---)
                     (C denotation v global-env loop))))))))



(define brown-load
  (lambda (file)
    (with-input-from-file file boot-tower)))

;;
(boot-tower)
