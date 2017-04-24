


;; spill phase code

;; eax ebx ecx edx esi edi esp ebp
;; eip also
;; 8 registers


;; appel N = 5 registers example

(use-modules (ice-9 pretty-print))

(define pp pretty-print)




;; introduce a , m variables at the start
(define prog '((set! a "a vector")
	       (set! m "return addr")
	       (set! b (vector-ref a 1))
	       (set! c (vector-ref a 2))
	       (set! d (vector-ref a 3))
	       (set! e (vector-ref a 4))
	       (set! f (vector-ref a 5))
	       (set! g (vector-ref a 6))
	       (set! h (+ b c))
	       (set! i (+ d e))
	       (set! j (+ f g))
	       (set! k (+ h i))
	       (set! l (+ k j))
	       (app m l)))




(define (set-expression? e)
  (and (pair? e)
       (= (length e) 3)
       (eq? (car e) 'set!)))

(define (vector-ref-expression? e)
  (and (pair? e)
       (= (length e) 3)
       (eq? (car e) 'vector-ref)))

(define (apply-expression? e)
  (and (pair? e)
       (= (length e) 3)
       (eq? (car e) 'app)))

(define (add-expression? e)
  (and (pair? e)
       (= (length e) 3)
       (eq? (car e) '+)))


(define (find-symbols e)
  (cond
   ((eq? e 'set!) '())
   ((eq? e 'app) '())
   ((eq? e '+) '())
   ((eq? e 'vector-ref) '())
   ((symbol? e) (list e))
   ((number? e) '())
   ((string? e) '())
   ((set-expression? e) (append (find-symbols (car (cdr e)))
				(find-symbols (car (cdr (cdr e))))))
   ((vector-ref-expression? e)
    (append (find-symbols (car (cdr e)))
	    (find-symbols (car (cdr (cdr e))))))
   ((apply-expression? e)
    (append (find-symbols (car (cdr e)))
				(find-symbols (car (cdr (cdr e))))))
   ((add-expression? e)
    (append (find-symbols (car (cdr e)))
				(find-symbols (car (cdr (cdr e))))))
   (else (error "find-symbols" e))))


;; symbol used as SRC source 
(define (src-symbols e)
  (cond
   ((eq? e 'set!) '())
   ((eq? e 'app) '())
   ((eq? e '+) '())
   ((eq? e 'vector-ref) '())
   ((symbol? e) (list e))
   ((number? e) '())
   ((string? e) '())
   ((set-expression? e) (append 
			 (src-symbols (car (cdr (cdr e))))))
   ((vector-ref-expression? e)
    (append (src-symbols (car (cdr e)))
	    (src-symbols (car (cdr (cdr e))))))
   ((apply-expression? e)
    (append (src-symbols (car (cdr e)))
	    (src-symbols (car (cdr (cdr e))))))
   ((add-expression? e)
    (append (src-symbols (car (cdr e)))
	    (src-symbols (car (cdr (cdr e))))))
   (else (error "src-symbols" e))))



;; symbol used as SRC source 
(define (dest-symbols e)
  (cond
   ((eq? e 'set!) '())
   ((eq? e 'app) '())
   ((eq? e '+) '())
   ((eq? e 'vector-ref) '())
   ((symbol? e) (list e))
   ((number? e) '())
   ((string? e) '())
   ((set-expression? e) (append (dest-symbols (car (cdr e)))))
   ((vector-ref-expression? e) '())
   ((apply-expression? e)  '())   
   ((add-expression? e) '())
   (else (error "dest-symbols" e))))



;; program is a sequence of commands
;; find all symbols on the command
(define (liveness program)
  (map (lambda (c)
	 (list c (dest-symbols c) (src-symbols c)))
       program))



;; remove duplicate symbols
(define (remove-duplicates xs)
  (cond
   ((null? xs) '())
   (else (remove-duplicates2 (car xs) (cdr xs)))))

(define (remove-duplicates2 x xs)
  (cond
   ((member x xs) (remove-duplicates xs))
   (else (cons x (remove-duplicates xs)))))



;; all symbols in program p
(define (all-symbols p)
  (remove-duplicates (apply append (map find-symbols p))))



;; find the lifetime of a variable over the program sequence
(define (life a p)
  (cond
   ((member a (dest-symbols (car p)))
    ;; yes -- not live until next command in program
    (append `(()) (life-last-use a (cdr p))))
   (else ;; no - not live yet
    (append '(()) (life a (cdr p))))))

(define (life-last-use a p)
  (cond
   ((member a (apply append (map src-symbols p))) ;; a still in use
    (append `((,a)) (life-last-use a (cdr p))))
   (else ;; no longer in use
    (life-after a p))))


(define (life-after a p)
  (cond
   ((null? p) '())
   (else (append '(()) (life-after a (cdr p))))))


;; (life 'a prog)
;; (life 'b prog)

;; find lifetime of all variables
(define (all-vars-life p)
  (map (lambda (sym) (life sym p)) (all-symbols p)))


;; see life times and program together
(define (combo p)
  (let ((av (all-vars-life p)))
    (map list (each av) prog)))


(define (each xs)
  (cond
   ((null? xs) '())
   ((null? (car xs)) '())
   (else (cons (apply append (map car xs))
	       (each (map cdr xs))))))



(define (test)
  (let ((x1 '(() (a) (a) (a) ()  ()))
	(x2 '(() ()  (b) (b) (b) ()))
	(x3 '(() ()  ()  (c) (c) (c))))
    (let ((xs (list x1 x2 x3)))
      xs)))


(define (test2)
  (let ((x1 '(() (a) (a) (a) ()  ()))
	(x2 '(() ()  (b) (b) (b) ()))
	(x3 '(() ()  ()  (c) (c) (c))))
    (let ((xs (list x1 x2 x3)))
      (map cdr xs))))
  
  
(define (test3)
  (let ((x1 '(() (a) (a) (a) ()  ()))
	(x2 '(() ()  (b) (b) (b) ()))
	(x3 '(() ()  ()  (c) (c) (c))))
    (let ((xs (list x1 x2 x3)))
      (apply append (map car xs)))))


(define (test4)
  (let ((x1 '(() (a) (a) (a) ()  ()))
	(x2 '(() ()  (b) (b) (b) ()))
	(x3 '(() ()  ()  (c) (c) (c))))
    (let ((xs (list x1 x2 x3)))
      (each xs))))

;; **** introduce a spill record and put all free variables into the record
;; n number of registers
;; p is the program
(define (needs-spill-help n cp)
  (cond
   ((null? cp) #f)
   ((> (length (car (car cp))) n) #t)
   (else (needs-spill-help n (cdr cp)))))


(define (spill? n p)
  (let ((cp (combo p))) ; get lifetimes
    (needs-spill-help n cp)))


;; ******* generate interference graph from liveness **********

;; (f g h i m) -> ((f g) (f h) (f i) (f m) (g h)(g i)(g m)(h i)(h m)(i m))
;; (f)
;; xs is a list of symbols 
(define (interfere xs)
  (cond
   ((null? xs) '()) ;; nothing to interfere
   ((null? (cdr xs)) '()) ;; no interference just itself
   (else (append (map (lambda (x) (list (car xs) x)) (cdr xs))
		 (interfere (cdr xs))))))


;; (all-symbols prog)
(define (interference p)
  (let ((lives (map car (combo p))))
    (remove-duplicate-edges (apply append (map interfere lives)))))


(define (remove-duplicate-edges xs)
  (cond
   ((null? xs) xs)
   (else (remove-duplicate-edges-helper (car xs) (cdr xs)))))


(define (remove-duplicate-edges-helper edge other-edges)
  (cond
   ((edge-member edge other-edges)
    (remove-duplicate-edges other-edges))
   (else (cons edge
	       (remove-duplicate-edges other-edges)))))


(define (edge-member e edges)
  (cond
   ((null? edges) #f)
   ((and (eq? (car e) (car (car edges)))
	 (eq? (car (cdr e)) (car (cdr (car edges)))))
    #t)
   ((and (eq? (car e) (car (cdr (car edges))))
	 (eq? (car (cdr e)) (car (car edges))))
    #t)
   (else (edge-member e (cdr edges)))))


;; ******************************************************************
;; for prog - simple program generates an interference graph
;;
;; nodes = (all-symbols prog)
;; = (a b c d e f g h i k j m l)
;;

;; (pp (interference prog))
;; ((a b)
;;  (a c)
;;  (a d)
;;  (a e)
;;  (a f)
;;  (a m)
;;  (b c)
;;  (b d)
;;  (b e)
;;  (b f)
;;  (b g)
;;  (b m)
;;  (c d)
;;  (c e)
;;  (c f)
;;  (c g)
;;  (c m)
;;  (d e)
;;  (d f)
;;  (d g)
;;  (d h)
;;  (d m)
;;  (e f)
;;  (e g)
;;  (e h)
;;  (e m)
;;  (f g)
;;  (f h)
;;  (f i)
;;  (f m)
;;  (g h)
;;  (g i)
;;  (g m)
;;  (h i)
;;  (h j)
;;  (h m)
;;  (i j)
;;  (i m)
;;  (k j)
;;  (k m)
;;  (j m)
;;  (m l))
;;
;;
;;

  






















    




