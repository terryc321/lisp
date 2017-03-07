

(define fac (lambda (n) (if (< n 2) 1 (* n (fac (- n 1))))))

;; map is a cps-primitive
map
;; but its a structure , we can pull it apart
(define map2 map)
(define twice (lambda (x) (+ x x)))

;; check that cps-primitives are first class
(map twice '(1 2 3 4 5))
(map2 twice '(1 2 3 4 5))
(eq? map map2)

;; can we make another map that uses the same cps-primitive
;;(define map3 (cps-primitive (cdr map2)))
;;map3



;; try swap definitions of fac and fib around
;; only change the <body>
;; we could if we so wish just exchange , entire definitions from params onward
(define swap-fib-fac
  (lambda ()
    (let ((fib-body (car (cdr (cdr fib))))
	  (fac-body (car (cdr (cdr fac)))))      
      (set-car! (cdr (cdr fib))	fac-body)
      (set-car! (cdr (cdr fac)) fib-body))))


(define show-definition
  (lambda (fun)
    (list (car fun) ;; lambda tag
	  (car (cdr fun)) ;; params
	  (car (cdr (cdr fun))) ;; body
	  '*an-env*)))

;; original fac
;; (lambda-tag3122 (n) ((if (< n 2) 1 (* n (fac (- n 1))))) *an-env*)
;; after swap-fib-fac
;; (lambda-tag3122 (n) ((if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2))))) *an-env*)

;; instrument factorial function to tell us how many times factorial was called
(define instr-fac #f)
(let ((counter 0))
  (letrec ((instr-fac-helper
	    (lambda (n)
	      (if (< n 2)
		  n
		  (* n (begin
			 (set! counter (+ 1 counter))
			 (instr-fac-helper (- n 1))))))))
    (set! instr-fac
	  (lambda (n)
	    (set! counter 0)	  
	    (let ((result (instr-fac-helper n)))
	      (newline)
	      (display "instr-fac called ")
	      (display counter)
	      (display " times")
	      (newline)
	      result)))))




(instr-fac 5)
(instr-fac 10)

;; test lambd-a arg patter matching
(define f (lambda args args))
(f 1 2 3 4 5 )

(define f2 (lambda (x . y) y))
(f2 'a 'b 'c 'd)

(define f3 (lambda (x y . z) z ))
(f3 'a 'b 'c 'd 'e 'f)

(define f4 (lambda (x y z) (list x y z)))
(f4 'a 'b 'c)

;; try putting a lambda around 

;; idea of redirection , similar to callcc
;;
;; fac ---> hook ---> do stuff --> hook_after
;;

fac

;; my procedure called
;;
;; change binding of fac to call fake-fac
;; except 
;; fake-fac n
(define fake-fac #f)
(define counter2 0)
(set! fake-fac (lambda (n)
		 (set! counter2 (+ counter2 1))
		 (fac n)))

(set! counter2 0)
(fac 10)
counter2

;; install instrumentation --- install binding in environment of fac lambda
;; fac
;;       params 
;; (lambda_tag (n)  ...body ....  environment )

;; or replace call to fac in the body of the lambda with call to fac-instr


;; need tools to explore environment more precisely
;; but this is definitely the way to go

;;(let ((fac-env (car (cdr (cdr (cdr fac))))))
;;  (set-car! fac-env (cons (list (cons fac (lambda (n) (fake-fac n))))
;;			  fac-env)))
;;			  (car (cdr (cdr (cdr fac)))))))


;; replace fac with instrumented fac completely
;; as its a top level form , global environment open to manipulation
(define fac instr-fac)		     
(fac 10)
;; to return factorial back to sanity , we can just redefine it again






		   


