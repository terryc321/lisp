
(define id
  (lambda (v) v))

;; identity continuation
(define kid
  (lambda (v k) (k v)))

;; call read --- assuming it parsed okay --- a big assumption ??
(define kread
  (lambda (k)
    (newline)
    (display "ready>")    
    (k (read))))



(define keval
  (lambda (exp env k kfail)    
    (cond
     ((boolean? exp) (k exp))
     ((number? exp) (k exp))
     ((quoted-expression? exp) (k (car (cdr exp))))
     ((eq? exp ':q) "bye bye") ;; escape the system entirely
     ((symbol? exp)   (klookup exp env k kfail))
     ((begin-expression? exp)      (kbegin exp env k kfail))
     ((list-expression? exp)      (klist exp env k kfail))
     ((cons-expression? exp) (kcons exp env k kfail))
     ((car-expression? exp)  (kcar exp env k kfail))
     ((cdr-expression? exp)  (kcdr exp env k kfail))     
     ((pair? exp)      (kapplication exp env k kfail))
     (else (kfail (list "keval: no understand" exp env k kfail))))))


(define klookup
  (lambda (exp env k kfail)
    (let ((key-val (assq exp env)))      
      (if key-val	  
	  (k (cdr key-val))
	  (kfail (list "klookup: no binding" exp env k kfail))))))

(define ksequence
  (lambda (exp env k kfail)
    ;;(display "ksequence =>")
    ;;(display exp)
    (cond
     ((and (pair? exp)
	   (null? (cdr exp)))
      ;;(display "ksequence last =>")
      ;;(display exp)
      (keval (car exp) env k kfail))
     ((null? exp)
      (k '()))
     (else
      (keval (car exp)
	  env
	  (lambda (v)
	    (ksequence (cdr exp) env k kfail))
	  kfail)))))

(define kbegin
  (lambda (exp env k kfail)
    ;;(display "kbegin =>")
    ;;(display exp)    
    (ksequence (cdr exp) env k kfail)))

(define begin-expression?
  (lambda (exp)
    (and (pair? exp)
	 (eq? (car exp) 'begin))))

(define list-expression?
  (lambda (exp)
    (and (pair? exp)
	 (eq? (car exp) 'list))))

;; (cons x y)
(define cons-expression?
  (lambda (exp)
    (and (pair? exp)
	 (eq? (car exp) 'cons)
	 (pair? (cdr exp))
	 (pair? (cdr (cdr exp)))
	 (null? (cdr (cdr (cdr exp)))))))


(define kcons
  (lambda (exp env k kfail)
    (keval (car (cdr exp))
	   env
	   (lambda (v1)
	     (keval
	      (car (cdr (cdr exp)))
	      env
	      (lambda (v2)
		(k (cons v1 v2)))
	      kfail))
	   kfail)))

;;------------------------------
;; (car x)
(define car-expression?
  (lambda (exp)
    (and (pair? exp)
	 (eq? (car exp) 'car)
	 (pair? (cdr exp))
	 (null? (cdr (cdr exp))))))

(define kcar
  (lambda (exp env k kfail)
    (keval (car (cdr exp))
	   env
	   (lambda (v1)
	     (if (pair? v1)
		 (k (car v1))
		 (kfail (list "kcar: not possible" v1))))
	   kfail)))



;;------------------------------
;; (cdr x)
(define cdr-expression?
  (lambda (exp)
    (and (pair? exp)
	 (eq? (car exp) 'cdr)
	 (pair? (cdr exp))
	 (null? (cdr (cdr exp))))))

(define kcdr
  (lambda (exp env k kfail)
    (keval (car (cdr exp))
	   env
	   (lambda (v1)
	     (if (pair? v1)
		 (k (cdr v1))
		 (kfail (list "kcdr: not possible" v1))))
	   kfail)))

;;------------------------------



(define klist
  (lambda (exp env k kfail) 
    (klist-accumulate '() (cdr exp) env k kfail)))
  
(define klist-accumulate
  (lambda (acc exp env k kfail)
    (cond
     ((null? exp)
      (kreverse acc '() k))
     (else
      (keval (car exp)
	  env
	  (lambda (v)
	    (klist-accumulate (cons v acc) (cdr exp) env k kfail))
	  kfail)))))

(define (kreverse xs ys k)
  (cond
   ((null? xs) (k ys))
   (else (kreverse (cdr xs) (cons (car xs) ys) k))))



(define kapplication
  (lambda (exp env k kfail)
    (kfail "not written kapplication yet.")))


       
(define kprint-error
  (lambda (v k)
    (display "ERROR::")
    (display v)
    (k v)))
  


(define kprint
  (lambda (v k)
    (display v)
    (k v)))


(define krepl
  (lambda (z)
    (kread (lambda (v)
	     (keval v '((a . 1)(b . 2)(c . 3)(d . 4)(z . 123))
		    (lambda (ev)
		      (kprint ev krepl))
		    (lambda (ev)
		      (kprint-error ev krepl)))))))



(define (quoted-expression? exp)
  (and (pair? exp)
       (eq? (car exp) 'quote)
       (not (null? (cdr exp)))
       (null? (cdr (cdr exp)))))

;; read something in and display it


(define k+
  (lambda (a b ks kf)
    (if (and (number? a) (number? b))
	(ks (+ a b))
	(kf (list "not a number" a b ks)))))

(define k*
  (lambda (a b ks kf)
    (if (and (number? a) (number? b))
	(ks (* a b))
	(kf (list "not a number" a b ks)))))


(define k<
  (lambda (a b ks kf)
    (if (and (number? a) (number? b))
	(ks (< a b))
	(kf (list "not a number" a b ks)))))

(define k>
  (lambda (a b ks kf)
    (if (and (number? a) (number? b))
	(ks (> a b))
	(kf (list "not a number" a b ks)))))

(define k=
  (lambda (a b ks kf)
    (if (and (number? a) (number? b))
	(ks (= a b))
	(kf (list "not a number" a b ks)))))




(define kloop
  (lambda (k)    
    (k kloop)))



(define k1 (lambda ( k)
	     (display "n1 = ")
	     (display n)
	     (newline)
	     (k 2)))

(define k2 (lambda ( k)
	     (display "n2 = ")
	     (display n)
	     (newline)
	     (k 3)))

(define k3 (lambda ( k)
	     (display "n3 = ")
	     (display n)
	     (newline)
	     (k 4)))

(define k4 (lambda ( k)
	     (display "n4 = ")
	     (display n)
	     (newline)
	     (k 5)))

(define k5 (lambda ( k)
	     (display "n5 = ")
	     (display n)
	     (newline)
	     (k 6)))

(define k6 (lambda ( k)
	     (display "n6 = ")
	     (display n)
	     (newline)
	     (k 7)))

(define k7 (lambda ( k)
	     (display "n7 = ")
	     (display n)
	     (newline)
	     (k 8)))


;; (define d
;;   (lambda (kont)
;;     ((lambda (n1)
;;        (display "n1 => ")
;;        (display  n1)
;;        (newline)
;;        ((lambda (n2)
;; 	  (display "n2 => ")
;; 	  (display  n2)
;; 	  (newline)
;; 	  ((lambda (n3)
;; 	     (display "n3 => ")
;; 	     (display  n3)
;; 	     (newline)
;; 	     (kthrow 3))
;; 	     ((lambda (n4)
;; 		(display "n4 => ")
;; 		(display  n4)
;; 		(newline)
;; 		((lambda (n5)
;; 		   (display "n5 => ")
;; 		   (display  n5)
;; 		   (newline)
;; 		   (s+ v 10 k)) 5))4))3))2))1))


