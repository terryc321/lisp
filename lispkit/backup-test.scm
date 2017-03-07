

;; want to load lispkit
(add-to-load-path (dirname (current-filename)))

;; require unit testing framework
;;(sfri sfri-64)
(use-modules (srfi srfi-64))
(use-modules (lispkit))

;;(test-log-to-file (open-output-file "my-log.log"))

;; (define-syntax test-eqv/m
;;   (syntax-rules ()
;;     ((_ fun args expect)
;;      (test-eqv (exec 'fun 'args) 'expect))))



(define any '())

;; Initialize and give a name to a simple testsuite.
(test-begin "test-stop")
(test-eqv (exec '(stop) '(b c))  '(b c))
(test-end "test-stop")


;; ldc
(test-begin "test-ldc")
(test-eqv (exec '(ldc a stop) any) 'a)
(test-end "test-ldc")

;; atom
(test-begin "test-atom")
(test-eqv (exec '(ldc a atom stop) any) #t)
(test-eqv (exec '(ldc (a) atom stop) any) #f)
(test-end "test-atom")


;; car
(test-begin "test-car")
(test-eqv (exec '(ldc (a) car stop) any) 'a)
(test-end "test-car")


(test-begin "test-cdr")
(test-equal (exec '(ldc (a b) cdr stop) any) '(b))
(test-eqv (exec '(ldc (a) cdr stop) any) '())
(test-end "test-cdr")



(test-begin "test-cons")
(test-equal '(a . b) (exec '(ldc a ldc b cons stop) any))
(test-equal '((1 . 2) 3 . 4) (secd '() '() (compil '(cons (cons 1 2) (cons 3 4))) '()))
(test-end "test-cons")


(test-begin "test-eq")
(test-equal (exec '(ldc a ldc b eq stop) any) #f)
(test-equal (exec '(ldc a ldc a eq stop) any) #t)
(test-equal (exec '(ldc b ldc b eq stop) any) #t)
(test-equal (exec '(ldc 1 ldc 1 eq stop) any) #t)
(test-equal (exec '(ldc 1 ldc 2 eq stop) any) #f)
(test-end "test-eq")

(test-begin "test-add")
(test-equal (exec '(ldc 271 ldc 127 add stop) any) 398)
(test-equal (exec '(ldc 1 ldc 2 add stop) any) 3)
(test-end "test-add")

(test-begin "test-sub")
(test-equal (exec '(ldc 10 ldc 8 sub stop) any) 2)
(test-equal (exec '(ldc 10 ldc 10 sub stop) any) 0)
(test-equal (exec '(ldc 1 ldc 2 sub stop) any) -1)
(test-end "test-sub")

(test-begin "test-mul")
(test-equal (exec '(ldc 271 ldc 127 mul stop) any) 34417)
(test-end "test-mul")

(test-begin "test-div")
(test-equal (exec '(ldc 512 ldc 256 div stop) any) 2)
(test-end "test-div")

(test-begin "test-rem")
(test-equal (exec '(ldc 271 ldc 127 rem stop) any) 17)
(test-end "test-rem")

(test-begin "test-leq")
(test-equal (exec '(ldc 271 ldc 127 leq stop) any) #f)
(test-equal (exec '(ldc 127 ldc 127 leq stop) any) #t)
(test-equal (exec '(ldc 127 ldc 271 leq stop) any) #t)
(test-end "test-leq")

(test-begin "test-sel")
(test-equal (exec '(ldc #t sel (ldc a stop) (ldc b stop)) any) 'a)
(test-equal (exec '(ldc #f sel (ldc a stop) (ldc b stop)) any) 'b)
(test-end "test-sel")

(test-begin "test-join")
(test-equal (exec '(ldc #t sel (ldc a join) (ldc b join) stop) any) 'a)
(test-equal (exec '(ldc #f sel (ldc a join) (ldc b join) stop) any) 'b)
(test-end "test-join")

(test-begin "test-ld")
(test-equal (secd '() '((a b c)) '(ld (1 . 1) stop) '())  'a)
(test-equal (secd '() '((a b c)) '(ld (1 . 2) stop) '())  'b)
(test-equal (secd '() '((a b c)) '(ld (1 . 3) stop) '())  'c)
(test-end "test-ld")



(test-begin "test-ldf")
;; load function -- ie makes a closure
;; since environment is empty then consing onto nil is just the list
(test-equal '((ldc a))
  (secd '((b c)) '() '(ldf (ldc a) stop) '()) )
(test-equal '((ldc a) . () )
  (secd '((b c)) '() '(ldf (ldc a) stop) '()) )
;; if environment is not empty ie -- here it has values ((p q r s))
;; then ldf will put code and environment together to form the closure
(test-equal '((ldc a) (p q r s))
  (secd '((b c)) '((p q r s)) '(ldf (ldc a) stop) '()))
(test-end "test-ldf")



(test-begin "test-ap")
(test-equal (exec '(ldf (ldc a stop) ap) '(b c)) 'a)
(test-end "test-ap")

(test-begin "test-rtn")
(test-equal (exec '(ldf (ldc a rtn) ap stop) '(b c)) 'a)
(test-end "test-rtn")

(test-begin "test-dum")
;;(test-equal (exec '(ldf (dum ld (1 . 1) rtn) ap stop) '((b c))) '(b c))
;;(test-equal (exec '(ldf (dum ld (1 . 2) rtn) ap stop) '((b c)(d e))) '(d e))
(test-end "test-dum")


(test-begin "test-rap")
(test-equal (exec '(dum ldf (ld (0 . 0) stop) rap) '((b c))) '(b c))
(test-end "test-rap")

(test-begin "test-compile-add")
(test-equal 3 (exec (compil '(add 1 2)) '()))
(test-equal 10 (exec (compil '(add (add 1 2) (add 3 4))) '()) )
(test-end "test-compile-add")

(test-begin "test-compile-let")
;;(test-equal 5 (exec (compil '(let x x 5)) any))
(test-end "test-compile-let")

(test-begin "test-nil")
(test-equal (exec '(nil stop) any) '())
(test-end "test-nil")

(test-begin "test-ld-basics")
(test-equal (secd '() '((a b c)) '(ld (1 . 1) stop) '()) 'a)
(test-equal (secd '() '((a b c)) '(ld (1 . 2) stop) '()) 'b)
(test-equal (secd '() '((a b c)) '(ld (1 . 3) stop) '()) 'c)
(test-end "test-ld-basics")

(test-begin "test-comp-add-var")
;; here provide runtime value of x = 3 into add x 1 with environment where x found at 1 . 1
(test-equal 4 (secd '() '((3)) (comp '(add x 1) '((x)) '(stop)) '()))
;; square x
(test-equal 25 (secd '() '((5)) (comp '(mul x x) '((x)) '(stop)) '()))
(test-end "test-comp-add-var")



(test-begin "test-comp-conditional")
;; test if 
(test-equal 1 (secd '() '((3)) (comp '(if #t 1 2) '() '(stop)) '()))
(test-equal 1 (secd '() '((3)) (comp '(if (leq 3 4) 1 2) '() '(stop)) '()))
(test-equal 2 (secd '() '((3)) (comp '(if (leq 4 3) 1 2) '() '(stop)) '()))
(test-end "test-comp-conditional")

(test-begin "test-comp-eq")
;; test eq
(test-equal #t (secd '() '((7 7)) (comp '(eq a a) '((a b)) '(stop)) '()))
(test-equal #f (secd '() '((7 9)) (comp '(eq a b) '((a b)) '(stop)) '()))
;; value of a is x , value of b is y
(test-equal #t (secd '() '((x y)) (comp '(eq a a) '((a b)) '(stop)) '()))
(test-equal #f (secd '() '((x y)) (comp '(eq a b) '((a b)) '(stop)) '()))
(test-end "test-comp-eq")


(test-begin "test-comp-cons")
;; test eq
(test-equal '(7 . 7) (secd '() '((7 7)) (comp '(cons a a) '((a b)) '(stop)) '()))
(test-equal '(7 . 9)  (secd '() '((7 9)) (comp '(cons a b) '((a b)) '(stop)) '()))
;; value of a is x , value of b is y
(test-equal '(x . x) (secd '() '((x y)) (comp '(cons a a) '((a b)) '(stop)) '()))
(test-equal '(x . y) (secd '() '((x y)) (comp '(cons a b) '((a b)) '(stop)) '()))
(test-end "test-comp-cons")



(test-begin "test-comp-carcdr")
;; test eq
(test-equal 7 (secd '() '((7 9)) (comp '(car (cons a a)) '((a b)) '(stop)) '()))
(test-equal 9 (secd '() '((7 9)) (comp '(cdr (cons a b)) '((a b)) '(stop)) '()))
;; value of a is x , value of b is y
(test-equal 'x (secd '() '((x y)) (comp '(car (cons a a)) '((a b)) '(stop)) '()))
(test-equal 'y (secd '() '((x y)) (comp '(cdr (cons a b)) '((a b)) '(stop)) '()))
(test-end "test-comp-carcdr")


(test-begin "test-comp-lambda")
;; lambda expression
(test-equal 7 (secd '()
		    '()
		    (compil '((lambda (x y) (add x y)) 3 4))
		    '())
	    )


(test-equal '(3 . 4)
  (secd '()
	'()
	(compil '((lambda (x y) (cons x y)) 3 4))
	'()))


(test-equal '(8 . 6)
  (secd '()
	'()
	(compil '(cons 8 6))
	'()))




;;(test-equal 7 (secd '() '((7 9)) (comp '((lambda (x y) (add x y)) 3 4) '((a b)) '(stop)) '()))
(test-end "test-comp-lambda")


(test-begin "test-comp-lambda2")

(test-equal 11
  (secd '()
	'()
	(compil '((lambda (x) x) 11))
	'()))

(test-equal 11
  (secd '()
	'()
	(compil '((lambda (x) x) 11))
	'()))

(test-end "test-comp-lambda2")


(test-begin "test-comp-let")

(test-equal 3 (secd '()
		    '()
		    (comp '(let ((x (add 1 2))) x)
			  '()
			  '(stop))
		    '()))

(test-equal 10 (secd '()
		    '()
		    (comp '(let ((x (add 1 2))
				 (y (add 3 4)))
			     (add x y))
			  '()
			  '(stop))
		    '()))

(test-equal 6 (secd '()
		    '((7 9))
		    (comp '(let ((x 1)(y 2)(z 3)) (add x (add y z)))
			  '((a b))
			  '(stop))
		    '()))

(test-equal 32 (secd '()
		    '()
		    (comp '(let ((x (add 1 2))
				 (y (add 30 5)))
			     (sub y x))
			  '()
			  '(stop))
		    '()))


(test-end "test-comp-let")




(test-begin "test-comp-letrec")

(test-equal 127 (secd
		 '()
		 '()
		 (comp '(letrec ((fac 127))
			  fac)
		       '()
		       '(stop))
		 '()))

(test-equal 5 (secd
		 '()
		 '()
		 (comp '(letrec ((fac (lambda (n) n)))
			  (fac 5))
		       '()
		       '(stop))
		 '()))


(test-equal 120 (secd
		 '()
		 '()
		 (comp '(letrec ((fac (lambda (n) (if (leq n 2) n (mul n (fac (sub n 1)))))))
			  (fac 5))
		       '()
		       '(stop))
		 '()))


(test-equal 55 (secd
		 '()
		 '()
		 (comp '(letrec
			    ((fib (lambda (n)
				    (if (eq n 1)
					n
					(if (eq n 2)
					    1
					    (add (fib (sub n 1))
						 (fib (sub n 2))))))))			  
			  (fib 10))
		       '()
		       '(stop))
		 '()))


(test-equal 55 (secd
		 '()
		 '()
		 (compil '(letrec
			      ((fib (lambda (n)
				      (if (eq n 1)
					  n
					  (if (eq n 2)
					      1
					      (add (fib (sub n 1))
						   (fib (sub n 2))))))))
			    (fib 10)))
		 '()))
;; 1 1 2 3 5 8 13 ....
;;                  55

(test-end "test-comp-letrec")


(test-begin "test-null")
(test-equal #t (secd '()
		     '()
		     (compil '(null '()))
		     '()))

(test-equal #t (secd '()
		     '()
		     (compil '(let ((n '())) (null n)))
		     '()))

(test-equal #f (secd '()
		     '()
		     (compil '(null 5))
		     '()))

(test-end "test-null")



(test-begin "test-comp-append")
(test-equal '(a b c d e f)
  (secd '()
	'()
	(compil '(letrec ((app (lambda (xs ys)
				 (if (null xs)
				     ys
				     (cons (car xs) (app (cdr xs) ys))))))
		   (app '(a b c) '(d e f))))
	'()))

(test-end "test-comp-append")


(test-begin "test-comp-reverse")
(test-equal '(z y x w v u t s r q p o n m l k j i h g f e d c b a)
  (secd '()
	'()
	(compil '(letrec ((rev (lambda (xs)
				 (if (null xs)
				     xs
				     (app (rev (cdr xs))
					  (cons (car xs) '())))))
			  (app (lambda (xs ys)
				 (if (null xs)
				     ys
				     (cons (car xs) (app (cdr xs) ys))))))
		   (rev '(a b c d e f g h i j k l m n o p q r s t u v w x y z))))
	'()))

(test-end "test-comp-reverse")


(test-begin "test-letrec-kogge")
;; (test-equal
;;     #f
;;     (compil '(letrec ((f (lambda (x m)
;; 			   (if (null x) m (f (cdr x) (add m 1))))))
;; 	       (f '(1 2 3) 0))))
(test-end "test-letrec-kogge")


(test-begin "test-letrec-kogge2")
(test-equal
    6
  (secd '()
	'()
	(compil '(let ((x 3)
		       (one 1))
		   (letrec ((fac (lambda (n m)
				   (if (eq n 0)
				       m
				       (fac (sub n 1) (mul n m))))))
		     (fac x one))))
	'()))

(test-end "test-letrec-kogge2")


(test-begin "test-object-code")
(test-equal    '(ldc a stop)  (compil '(quote a)))
(test-equal    '(ldc a car stop)  (compil '(car (quote a))))
(test-equal    '(ldc a cdr stop)  (compil '(cdr (quote a))))
(test-equal    '(ldc a atom stop)  (compil '(atom (quote a))))
(test-equal    '(ldc a ldc b cons stop)  (compil '(cons 'a 'b)))

(test-equal    '(ldc a ldc b add stop)  (compil '(add 'a 'b)))
(test-equal    '(ldc a ldc b sub stop)  (compil '(sub 'a 'b)))
(test-equal    '(ldc a ldc b mul stop)  (compil '(mul 'a 'b)))
(test-equal    '(ldc a ldc b div stop)  (compil '(div 'a 'b)))

(test-equal    '(ldc a ldc b rem stop)  (compil '(rem 'a 'b)))
(test-equal    '(ldc a ldc b leq stop)  (compil '(leq 'a 'b)))

(test-equal    '(ldf (ldc a rtn) stop) (compil '(lambda (x) 'a)))
;; debrujn indices
(test-equal    '(ldf (ld (1 . 1) rtn) stop) (compil '(lambda (x) x)))

(test-equal  '(ldf (ld (1 . 2) rtn) stop) (compil '(lambda (x y) y)))

(test-equal '(ldc a ldc () cons ldf (ld (1 . 1) rtn) ap stop)
  (compil '((lambda (x) x) 'a)))

(test-equal '(ldc a ldc () cons ldf (ld (1 . 1) rtn) ap stop)
  (compil '(let ((x 'a)) x)))

(test-equal
    '(dum ldc a ldc () cons ldf (ld (1 . 1) rtn) rap stop)
    (compil '(letrec ((x 'a)) x)))


(test-equal
    '(ldc a sel (ldc b join) (ldc c join) stop)
    (compil '(if 'a 'b 'c)))



(test-end "test-object-code")


(test-begin "test-identity")
(test-equal 5
  (secd '() '() (compil '((lambda (x) x) 5)) '()))

(test-equal 12
  (secd '() '() (compil '(let ((id (lambda (x) x)))
			   (id 12))) '()))

(test-end "test-identity")

;; (test-equal 55 (secd
;; 		 '()
;; 		 '()
;; 		 (compil '(letrec
;; 			      ((rev (lambda (n)
;; 				      (if (null n)
;; 					  n
;; 					  (if (eq n 2)
;; 					      1
;; 					      (add (fib (sub n 1))
;; 						   (fib (sub n 2))))))))
;; 			    (fib 10)))
;; 		 '()))
;;(test-end "test-comp-reverse")

;; (define v (make-vector 5 99))
;; ;; Require that an expression evaluate to true.
;;(test-assert #f)

;; (test-assert (vector? v))
;; ;; Test that an expression is eqv? to some other expression.
;; (test-eqv 99 (vector-ref v 2))
;; (vector-set! v 2 7)
;; (test-eqv 7 (vector-ref v 2))
;; Finish the testsuite, and report results.
(test-begin "test1")
;; no tests
(test-end "test1")





;; (exec '(stop) '(b c)) ;; expect '((b c))
;; (exec '(ldc a stop) '(b c)) ;; expect a
;; (exec '(ldc a atom stop) '()) ;; expect true





