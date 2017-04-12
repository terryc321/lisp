
;; http://matt.might.net/articles/by-example-continuation-passing-style/
;; code due to matt might , adapted for basic scheme

;;
;; call/cc => (lambda (f cc) (f (lambda (x k) (cc x)) cc))
;;


;;
;; (define (cps-convert term cont)
;;   (match term;
;     [`(,f ,e)
;;      ; =>
;;      (let (($f (gensym 'f))
;;            ($e (gensym 'e)))
;;        (cps-convert f `(lambda (,$f)
;;          ,(cps-convert e `(lambda (,$e)
;;              (,$f ,$e ,cont))))))]
;;     [`(lambda (,v) ,e)
;;      ; =>
;;      (let (($k (gensym 'k)))
;;        `(,cont (lambda (,v ,$k)
;;                  ,(cps-convert e $k))))]
;;     [(? symbol?)
;;      ; =>
;;      `(,cont ,term)]))
;;
;; (define (cps-convert-program term)
;;   (cps-convert term '(lambda (ans) ans)))



(define (cps-convert term cont)
  (display "cps-convert : term => ") (display term) (newline)
  (cond
   ((symbol? term) (list cont term))
   ((not (pair? term)) (error "term is not symbol or pair - cpsconvert" term cont))
   ((eq? (car term) 'lambda)
    (let ((var (car (car (cdr term))))
	  (expr (car (cdr (cdr term))))
	  ($k (gensym "k")))
      (list cont (list 'lambda (list var $k)
		       (cps-convert expr $k)))))
   (else ; application (fn expr)
    (let ((fn (car term))
	  (expr (car (cdr term)))
	  ($f (gensym "f"))
	  ($e (gensym "e")))
      (cps-convert fn (list 'lambda (list $f)
			    (cps-convert expr (list 'lambda (list $e)
						    (list $f $e cont)))))))))
    
    
(define (cps-convert-program term)
  (cps-convert term '(lambda (ans) ans)))


