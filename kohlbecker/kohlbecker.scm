
;; http://web.cs.ucdavis.edu/~devanbu/teaching/260/kohlbecker.pdf
;; kohlbecker hygiene algorithm

;; record-case macro --- no implementations provide this yet.
;;    can rewrite record case macro ??
;;
;; symbol property lists -- no implementations provide this yet.
;;    can they be simulated using external hash table
;;


;; from page 160
(define Ehyg
  (lambda (s)
    (lambda (theta)
      (U (A (((E ((T s) S-naught)) theta) 1))))))


(define T
  (lambda (t)
    (lambda (tau)
      (cond
       ((atomic-non-var? t) t)
       ((var? t) (tau t))
       (else (map (lambda (t) ((T t) tau)) t))))))


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
	  '(LAMBDA ,(var t)
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
      (let ((v (gensym (U (var t)) ":" "new")))
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
	      (let ((new (gensym v ":" n)))
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
(define const? number?)
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
  (lambda (n)
    (record-case n
		 (LET (var val body) #t)
		 (IF (a b c) #t)
		 (OR (a b) #t)
		 (NAIVE-OR (a b) #t)
		 (FAKE (x) #t)
		 (CASE (a b) #t)
		 (else #f))))

(define ST
  (lambda (m)
    (record-case m
		 (LET (i e b) `((LAMBDA ,i ,b) ,e))
		 (IF (a b c) `(((ef ,a) ,b) ,c))
		 (OR (a b) `(LET v ,a (IF v v ,b)))
		 (NAIVE-OR (a b)
			   (let ((v (S-naught 'v)))
			     `(LET ,v ,a (IF ,v ,v ,b))))
		 (FAKE (x) `(QUOTE ,x))
		 (CASE (exp pair)
		       `(LET v ,exp
			     (IF ((eq? v) (QUOTE ,(car pair)))
				 ,(cadr pair)
				 #f)))
		 (else (error "syntax table: no match" m)))))



((Ehyg '(LET x (OR a v) (NAIVE-OR x v))) ST)

((Ehyg '(LAMBDA a (CASE (FAKE a) (QUOTE a)))) ST)














