
;; http://web.cs.ucdavis.edu/~devanbu/teaching/260/kohlbecker.pdf
;; kohlbecker hygiene algorithm

;; record-case macro --- no implementations provide this yet.
;;    can rewrite record case macro ??
;;
;; symbol property lists -- no implementations provide this yet.
;;    can they be simulated using external hash table
;;

(use-modules (ice-9 pretty-print))
(define pp pretty-print)
(define debug (lambda args (format #t "db: ~a ~%" args)))
(define put set-symbol-property!)
(define get symbol-property)
;; i dont think it means gensym , really string->symbol
;; make-symbol
;;(define gensym3 (lambda (a b c) (make-symbol (format #f "~a~a~a" a b c))))
;; gensym in guile says it may be unique , whereas make symbol ensures its unique.
(define gensym3 (lambda (a b c)
		  (let ((str (format #f "~a~a~a" a b c)))
		    (let ((sym (make-symbol str)))
		    (put sym 'pretty str)
		    sym))))

(define add1 1+)

;; * step 1 *
;; process stree with function T
;; stamp all leaves with function tau
;;  initially tau = S 0 or (S 0)
;; result is a time stamped syntax tree.
;; s is original s expression
;; ((T s) S-naught)
;; time stamp s expression with S-0

;; * step 2 *
;; tsstree = time stamped s tree
;; function E traverses through tsstree when finds something to expand
;; it does the transcription
;; but also applies ((T _) (S j))  where j is clock value
;; afterwards the clock value increases and expansion continues
;;


;; theta :  syntax table 
;; from page 160
(define Ehyg
  (lambda (s)
    (debug (format #f "Ehyg given s = ~A" s))
    (lambda (theta)
      (debug (format #f "Ehyg(s=~a) given theta = ~a" s theta))      
      (U (A (((E
	       ((T s) S-naught))
	      theta)
	     1))))))




(define T
  (lambda (t)
    (debug (format #f "T given t = ~a" t))
    (lambda (tau)
      (debug (format #f "T(t=~a) given tau = ~A" t tau))
      (cond
       ((atomic-non-var? t)
	(debug (format #f "T ~a : atomic-non-var? = #t" t))
	t)
       ((var? t)
	(debug (format #f "T ~a : var? = #t" t))
	(let ((temp (tau t)))
	  (debug (format #f "T tau(t=~a) = ~a" t temp))	
	  temp))
       (else
	(debug (format #f "T ~a : not atomic-non-var , not var? , so.. map over (t = ~a)" t t))
	(let ((temp
	       (map
		(lambda (v) ((T v) tau))
		t)))
	  (debug (format #f "T map over t (t=~a) = ~a" t temp))
	  temp))))))




;; E is macro expander , macro expand
;; t     : time stamped s expression
;; theta : syntax table
;; j     : clock value
(define E
  (lambda (t)
    (lambda (theta)
      (lambda (j)
	(cond
	 ((const? t) t)
	 ((stamped? t) t)
	 ((quote? t) t)
	 ((macro? t)
	  (((E ((T (theta t)) (S j))) theta)
	   (add1 j)))
	 ((lambda? t)
	  `(LAMBDA ,(var t)
		   ,(((E (body t)) theta) j)))
	 ((app? t)
	  `(,(((E (fun t)) theta) j)
	    ,(((E (arg t)) theta) j))))))))




(define A
  (lambda (t)
    (cond
     ((var? t) t)
     ((atomic-non-var? t) t)
     ((quote? t) t)
     ((lambda? t)
      (let ((v (gensym3 (U (var t)) ":" "new")))
	`(LAMBDA ,v
		 ,(A ((*/* v (var t))
		      (body t))))))
     ((app? t)
      `(,(A (fun t))
	,(A (arg t)))))))



(define U
  (lambda (t)
    (cond
     ((atomic-not-stamped? t) t)
     ((stamped? t)
      (get t 'original-name))
     (else (map U t)))))

(define S
  (lambda (n)
    (let ((seen '()))
      (lambda (v)
	(let ((info (assq v seen)))
	  (if info
	      (cdr info)
	      (let ((new (gensym3 v ":" n)))
		(put new 'original-name v)
		(set! seen
		  (cons
		   (cons v new) seen))
		new)))))))


(define S-naught (S 0))

(define */*
  (lambda (v w)
    (lambda (t)
      (cond
       ((stamped? t) (if (eq? t w) v t))
       ((atomic-not-stamped? t) t)
       ((quote? t) t)
       ((lambda? t)
	(if (eq? w (var t))
	    `(LAMBDA ,w ,(body t))
	    `(LAMBDA ,(var t)
		     ,((*/* v w)
		       (body t)))))
       ((app? t)
	`(,((*/* v w) (fun t))
	  ,((*/* v w) (arg t))))))))


(define stamped?
  (lambda (w)
    (and (symbol? w)
	 (get w 'original-name))))


(define mactok?
  (lambda (m)
    (and (symbol? m)
	 (get m 'mactok))))


(define coretok?
  (lambda (c)
    (and (symbol? c)
	 (get c 'coretok))))

(define quote?
  (lambda (t)
    (and (pair? t)
	 (eq? (car t) 'QUOTE)
	 (pair? (cdr t))
	 (null? (cddr t)))))

(define lambda?
  (lambda (t)
    (and (pair? t)
	 (eq? 'LAMBDA (car t))
	 (pair? (cdr t))
	 (var? (cadr t))
	 (pair? (cddr t))
	 (null? (cdddr t)))))

(define app?
  (lambda (t)
    (and (pair? t)
	 (pair? (cdr t))
	 (null? (cddr t)))))


(define atomic-non-var?
  (lambda (y)
    (or (const? y)
	(stamped? y)
	(mactok? y)
	(coretok? y))))

(define atomic-not-stamped?
  (lambda (x)
    (or (const? x)
	(and (var? x)
	     (not (stamped? x)))
	(mactok? x)
	(coretok? x))))



(define var? symbol?)
(define const? (lambda (v) (or (number? v) (boolean? v))))
(define var cadr)
(define body caddr)
(define fun car)
(define arg cadr)

(put 'LAMBDA 'coretok #t)
(put 'QUOTE 'coretok #t)
(put 'LET 'mactok #t)
(put 'IF 'mactok #t)
(put 'OR 'mactok #t)
(put 'NAIVE-OR 'mactok #t)
(put 'FAKE 'mactok #t)
(put 'CASE 'mactok #t)

(define macro?
  (lambda (n) (if (and (pair? n)
		  (memq (car n) '(LET IF OR NAIVE-OR FAKE CASE)))
		   #t
		   #f)))



(define ST
  (lambda (m)
    (cond
     ;; LET in terms of LAMBDA 
     ((eq? (car m) 'LET)
      (let ((i (car (cdr m)))
	    (e (car (cdr (cdr m))))
	    (b (car (cdr (cdr (cdr m))))))
	`((LAMBDA ,i ,b) ,e)))
     ;; IF in terms of dont really know 
     ((eq? (car m) 'IF)
      (let ((a (car (cdr m)))
	    (b (car (cdr (cdr m))))
	    (c (car (cdr (cdr (cdr m))))))
	`(((ef ,a) ,b) ,c)))
     ;; OR in terms of LET and IF
     ((eq? (car m) 'OR)
      (let ((a (car (cdr m)))
	    (b (car (cdr (cdr m)))))
	`(LET v ,a (IF v v ,b))))
     ;; NAIVE-OR
     ((eq? (car m) 'NAIVE-OR)
      (let ((a (car (cdr m)))
	    (b (car (cdr (cdr m)))))
	(let ((v (S-naught 'v)))
	  `(LET ,v ,a (IF ,v ,v ,b)))))
     ;; FAKE
     ((eq? (car m) 'FAKE)
      (let ((x (car (cdr m))))
	`(QUOTE ,x)))
     ;; CASE -- do we wish to say #f here ??
     ((eq? (car m) 'CASE)
      (let ((exp (car (cdr m)))
	    (pr (car (cdr (cdr m)))))      
	`(LET v ,exp
	      (IF ((eq? v) (QUOTE ,(car pr)))
		  ,(cadr pr)
		  #f))))
     (else (error "syntax table: no match" m)))))



(define (ppp expr)
  (cond
   ((pair? expr) (map ppp expr))
   ((and (symbol? expr)
	 (get expr 'pretty))
    (get expr 'pretty))
   (else expr)))






    



;;((Ehyg '(LAMBDA a (CASE (FAKE a) (QUOTE a)))) ST)

;; (let ((s '(LET x (OR a v) (NAIVE-OR x v))))
;;   ((T s) S-naught))


;; (let ((s '(LET x (OR a v) (NAIVE-OR x v)))
;;       (theta ST))
;;   (((E ((T s) S-naught))
;;     theta)
;;    1))



;;((Ehyg '(LAMBDA x (LAMBDA x ((f x) x)))) ST)
;;((Ehyg '(LET x (OR a v) (NAIVE-OR x v))) ST)

(define demo0 (ppp
	       ((Ehyg '(LAMBDA x (LAMBDA x ((f x) x)))) ST)))


;; prettify un-interned symbols
(define demo1 (ppp
	       ((Ehyg '(LET x (OR a v) (NAIVE-OR x v))) ST)))

(define demo2 (ppp
	       ((Ehyg '(LAMBDA a (CASE (FAKE a) (QUOTE a)))) ST)))






















