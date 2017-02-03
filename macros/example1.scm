#lang r5rs

;;(import (rnrs mutable-pairs (6)))
;;(require scheme/mpair)

;; drracket macro stepper but own ideas about cons and concerns of optimizations
(define-syntax push
  (syntax-rules ()
    ((push item place)
     (set! place (cons item place)))))

(define example1
  (let* ((cons (lambda (name)
                 (case name
                   ((phil) '("three card monte"))
                   ((dick) '("secret plan to end the war"
                             "agnew"
                             "not a crook"))
                   ((jimmy) '("why not the best"))
                   ((ron) '("abolish the draft"
                            "balance the budget"))
                   (else '()))))
         (scams (cons 'phil)))
    (push (car (cons 'jimmy)) scams)
    (push (cadr (cons 'ron)) scams)
    scams))

;; simplified form of setf common lisp macro
(define example2
  (let-syntax
      ((set! (syntax-rules (car cdr vector-ref)
               ((set! (car x) y) (set-car! x y))
               ((set! (cdr x) y) (set-cdr! x y))
               ((set! (vector-ref x e) y) (vector-set! x e y))
               ((set! x y) (set! x y)))))
    (let* ((days (list 'monday 'wednesday 'friday))
           (day1 'sunday))
      (set! (car days) 'tuesday)
      (set! day1 (car days))
      day1)))

(define example3
  (let ((car cdr)
        (set-car! set-cdr!)
        (cdr car)
        (set-cdr! set-car!))
    (let-syntax
        ((set! (syntax-rules (car cdr vector-ref)
                 ((set! (car x) y) (set-car! x y))
                 ((set! (cdr x) y) (set-cdr! x y))
                 ((set! (vector-ref x e) y) (vector-set! x e y))
                 ((set! x y) (set! x y)))))
      (let* ((days (list 'monday 'wednesday 'friday))
             (set-car! (lambda () 17)))
        (set! (car days) 'tuesday)
        (cons (set-car!) days)))))

