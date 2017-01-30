; The code for the article 
; "Scheme procedure -> R5RS macro, by example of quotify"

; The following is a Bigloo-specific module declaration.
; Please replace with appropriate inclusion form if using Scheme48
; or other Scheme system.
; The rest of the code is fully R5RS-compliant.
(module aaa
	(include "macro-lambda.scm")
)

; The idea is that the algorithm can be developed and tested in the
; procedural env, and only then converted to a macro.
; Macros are notoriously difficult to debug
; The final R5RS macro is modular -- and built from separate small pieces,
; that can be developed and tested independently.

; The code was written largely  mechanically, 
; when I was tired later last night.
; All R5RS macros worked either the first time, or after I fixed a few
; typos and other minor errors (like typing kt when I meant kf).
; Usually R5RS macros require far more debugging then correcting
; spelling errors.

; Function to selectively modify specific nodes in a tree
; selectively unquote symbols
; (quotify-fn '(i j k) '(4 k 5 l () (m i) ((('i)))))
; ==> `(4 ,k 5 l () (m ,i) (((',i))))

; First we write the function

(define (quotify-fn symbs-l tree)
  (define (doit tree)
    (map 
     (lambda (node)
       (if (pair? node)
	 ; recurse to process children
	   (doit node)
	   (if (memq node symbs-l)
	     ; replace the leaf
	       (list 'unquote node)
	       node)))
     tree))
  (list 'quasiquote (doit tree)))

(display (quotify-fn '(i j k) '(4 k 5 l () (m i) ((('i))))))
(newline)

; gsi gives `(4 ,k 5 l () (m ,i) (((',i))))
; bigloo gives the same 

; now we make it a macro

(define-macro (quotify-ll symbs-l tree)
  (define (doit tree)
    (map 
     (lambda (node)
       (if (pair? node)
	 ; recurse to process children
	   (doit node)
	   (if (memq node symbs-l)
	     ; replace the leaf
	       (list 'unquote node)
	       node)))
     tree))
  (list 'quasiquote (doit tree)))

; Test
(let ((i 'symbol-i) (j "str-j") (k "str-k"))
  (display (quotify-ll (i j k) (4 k 5 l () (m i) ((('i))))))
  (newline))
; (4 str-k 5 l () (m symbol-i) ((('symbol-i)))) 

; R5RS macros

; first, we re-write quotify-fn in CPS
; We need CPS versions of map, memq, etc. predicates
; We will see the need for such low-level detailization

; -- primitives
(define (cons-cps a b k)
  (k (cons a b)))

(define (car-cps x k)
  (k (car x)))

(define (cdr-cps x k)
  (k (cdr x)))

; Check to see if x is a pair. If it is, apply kt to x; otherwise,
; apply kf to x
(define (ifpair? x kt kf)
  (if (pair? x) (kt x) (kf x)))

; if (eq? a b) ==> apply kt to a
; otherwise, apply kf to the pair of (a b) 
(define (ifeq? a b kt kf)
  (if (eq? a b) (kt a) (kf a)))

;-- more complex terms
; if a occurs in lst, pass the sublist to kt. Otherwise, pass () to kf
(define (memq-cps a lst kt kf)
  (ifpair? lst
    (lambda (lst) ; it's a pair
      (car-cps lst
	(lambda (x)
	  (ifeq? a x
	    ; match
	    (lambda (_) (kt lst))
	    ; mismatch
	    (lambda (_)
	      (cdr-cps lst
		(lambda (tail)
		  (memq-cps a tail kt kf))))))))
    (lambda (empty)
      (kf empty))))

(memq-cps 'i '(j k l)
	  (lambda (r) (display "OK:") (display r) (newline))
	  (lambda (r) (display "FAIL:") (display r) (newline))
)
(memq-cps 'i '(j i l)
	  (lambda (r) (display "OK:") (display r) (newline))
	  (lambda (r) (display "FAIL:") (display r) (newline))
)

      
; Map a CPS function f on a list
; map f () k => k ()
; map f (x . tail) k => (f x (lambda (fx) (map f tail (lambda (new-tail))
; (k (fx . new-tail)))

(define (map-cps f lst k)
  (ifpair? lst
     ; lst still has elements
     (lambda (lst)
       (car-cps lst
          (lambda (x)
            (f x
               (lambda (fx)
                 (cdr-cps lst
                    (lambda (tail)
                      (map-cps f tail
                               (lambda (res)
                                 (cons-cps fx res k))))))))))
     ; lst is empty
     (lambda (empty)
       (k empty))))

; Test code
(map-cps (lambda (x k) (k (+ 1 x))) '(1 2 3 4) display)
(newline)

(define (quotify-cps symbs-l tree k)
  (define (doit tree k)
    (map-cps
     (lambda (node k)
       (ifpair? node
         ; recurse to process children
         (lambda (node1)
           (doit node1 k))
         (lambda (node1) ; node is not a pair
           (memq-cps node1 symbs-l
             ; matches
             (lambda (_) (k (list 'unquote node1)))
             ; mis-matches: leave the node alone
             (lambda (_) (k node1))))))
     tree
     k))
  (doit tree (lambda (conv-tree) (k (list 'quasiquote conv-tree)))))

(quotify-cps '(i j k) '(4 k 5 l () (m i) ((('i)))) display)
(newline)

; Step II: macroization

; macro-lambda.scm is included...

; cons in CPS
; ?cons A LST K
; pass (A . LST) to K

(define-syntax ?cons
  (syntax-rules ()
    ((_ x y k)
     (??!apply k (x . y)))))

; append in CPS
; ?append LIST1 LIST2 K
; pass (append LIST1 LIST2) to K

(define-syntax ?append 
  (syntax-rules ()
    ((_ () x k)
     (??!apply k x))
    ((_  x () k)
     (??!apply k x))
    ((_ (x ...) (y ...) k)
     (??!apply k (x ... y ...)))))

; (?append (a b c) (d)
;  (??!lambda (result) (display '(??! result)))
; )
; (newline)

; map in CPS
; ?map F (X Y ...) K
; pass ((f X) (f Y) ...) to K
; Here F is a CPS (syntax) function F X KX that passes (f x) to KX

; (define-syntax ?map
;   (syntax-rules ()
;     ((_ f () k)
;      (??!apply k ()))
;     ((_ f (x . rest) k)
;      (f x
;       (??!lambda (new-x)
; 	 (?map f rest
; 	   (??!lambda (new-rest)
; 	     (?cons (??! new-x) (??! new-rest) k))))))))

; Check to see if x is a pair. If it is, apply kt to x; otherwise,
; apply kf to x
(define-syntax ?ifpair? 
  (syntax-rules ()
    ((_ (a . b) kt kf)
     (??!apply kt (a . b)))
    ((_ non-pair kt kf)
     (??!apply kf non-pair))))

(define-syntax ?car
  (syntax-rules ()
    ((_ (x . y) k) (??!apply k x))))

(define-syntax ?cdr
  (syntax-rules ()
    ((_ (x . y) k) (??!apply k y))))


; if (eq? a b) ==> apply kt to a
; otherwise, apply kf to a
; Here we cut corners a little bit: if 'a' is not a symbol, 
; we always assume mismatch. R5RS gives an implementation a freedom
; to eq? numbers and literal pairs and strings as the implementation
; sees fit.
; This is the only piece that requires care. But i's small, and can be
; debugged separately -- independent of other code.
(define-syntax ?ifeq?
  (syntax-rules ()
    ((_ (x . y) b kt kf) ; a is not a symbol: always false
     (??!apply kf (x . y)))
    ((_ () b kt kf) ; a is not a symbol: always false
     (??!apply kf ()))
    ((_ a b _kt _kf)
     (let-syntax
	 ((aux 
	   (syntax-rules (a)
	     ((_ a kt kf)
	      (??!apply kt a))
	     ((_ other kt kf)
	      (??!apply kf a)))))
       (aux b _kt _kf)))))

(?ifeq? i i 
       (??!lambda (r) (begin (display "OK:") (display '(??! r)) (newline)))
       (??!lambda (r) (begin (display "FAIL:") (display '(??! r)) (newline)))
)
(?ifeq? i j
       (??!lambda (r) (begin (display "OK:") (display '(??! r)) (newline)))
       (??!lambda (r) (begin (display "FAIL:") (display '(??! r)) (newline)))
)


;-- more complex terms
; if a occurs in lst, pass the sublist to kt. Otherwise, pass () to kf
(define-syntax ?memq 
  (syntax-rules ()
    ((_ a _lst kt kf)
     (?ifpair? _lst
	(??!lambda (lst) ; it's a pair
	 (?car _lst
	  (??!lambda (x)
	   (?ifeq? a (??! x)
	    ; match
	    (??!lambda (_) (??!apply kt (??! lst)))
	    ; mismatch
	    (??!lambda (_)
	      (?cdr _lst
		(??!lambda (tail)
		  (?memq a (??! tail) kt kf))))))))
    (??!lambda (empty)
      (??!apply kf (??! empty)))))))


(?memq i (j k l)
       (??!lambda (r) (begin (display "OK:") (display '(??! r)) (newline)))
       (??!lambda (r) (begin (display "FAIL:") (display '(??! r)) (newline)))
)
(?memq i (j i l)
       (??!lambda (r) (begin (display "OK:") (display '(??! r)) (newline)))
       (??!lambda (r) (begin (display "FAIL:") (display '(??! r)) (newline)))
)

(define-syntax ?map
  (syntax-rules ()
    ((?map f lst k)
     (?ifpair? lst
        ; lst still has elements
        (??!lambda (lst1)
          (?car (??! lst1)
            (??!lambda (x)
              (??!apply f (??! x) 
	        (??!lambda (fx)
	          (?cdr (??! lst1)
		    (??!lambda (tail)
		      (?map f (??! tail)
		        (??!lambda (res)
			   (?cons (??! fx) (??! res) k))))))))))
        ; lst is empty
	(??!lambda (empty)
	  (??!apply k (??! empty)))))))

(?map
 (??!lambda (x k)
	    (??!apply (??! k) (+ 1 (??! x))))
      (1 2 3 4)
      (??!lambda (x) (display '(??! x))))
(newline)


(define-syntax ?quotify
  (syntax-rules (quasiquote unquote)
  ((_ symbs-l _tree _k)
   (letrec-syntax
       ((doit
         (syntax-rules ()
           ((_ tree k)
            (?map 
	     (??!lambda (node mk)
		(?ifpair? (??! node)
                                        ; recurse to process children
                (??!lambda (node1)
                    (doit (??! node1) (??! mk)))
                (??!lambda (node1) ; node is not a pair
                    (?memq (??! node1) symbs-l
                                        ; matches
                      (??!lambda (_) (??!apply (??! mk) (unquote (??! node1))))
                                        ; mis-matches: leave the node alone
                      (??!lambda (_) (??!apply (??! mk) (??! node1)))))))
		tree k)))))
     (doit _tree
           (??!lambda (conv-tree)
                      (??!apply _k (quasiquote (??! conv-tree)))))))))

; (?quotify (i j k) (4 k 5 l () (m i) ((('i))))
; 	  (??!lambda (r) (display '(??! r))))
; (newline)

; it may take a while ...

; Finally, wrap a CPS macro ?quotify in a non-CPS macro so it would be
; easier to use. The wrapping will make the code look prettier.
; If we need to compose with other macros, the unwrapped ?quotify
; macro should be used.

; Note how generic this wrapper is
(define-syntax quotify
  (syntax-rules ()
    ((_ args ...)
     (?quotify args ...
	       ; the identity continuation: (lambda (x) x)
	    (??!lambda (r) (begin (??! r)))))))
   
(let ((i 'symbol-i) (j "str-j") (k "str-k"))
  (display (quotify (i j k) (i4 k i5 l () (m i) ((('i))))))
  (newline))
; (i4 str-k i5 l () (m symbol-i) ((((quote symbol-i))))) 

