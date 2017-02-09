#lang racket

;;---------------------------------------------------------------------
;; record-case
;;(include "recscm.scm")
;;(require "recscm.scm")
;;(require (planet dvanhorn/record-case:1:1/record-case))


;; gensym in racket ?
;; property lists on symbols ?? put

(define *symbol-properties* (make-hash))

(define (get item property)
  (let ((sh (hash-ref *symbol-properties* item #f)))
    (if sh
        (begin  (hash-ref sh property #f))
        (begin #f))))

(define (put item property value)
  (let ((sh (hash-ref *symbol-properties* item #f)))
    (if sh
        (begin		; its here
          (hash-set! sh property value)
          )
        (begin ; its not
          (let ((sh (make-hash)))
            (hash-set! *symbol-properties* item sh)
            (hash-set! sh property value))))))


(define generate-symbol (lambda (a b c) (gensym (format "~a~a~a" a b c))))

;;---------------------------------------------------------------------




;; http://web.cs.ucdavis.edu/~devanbu/teaching/260/kohlbecker.pdf
;; kohlbecker hygiene algorithm


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
       (let ((v (generate-symbol (U (var t)) ":" "new")))
         `(LAMBDA ,v
                  ,(A ((*/* v (var t))
                       (body t))))))
      ((app? t)
       `(,(A (fun t))
         ,(A (arg t)))))))



(define U
  (lambda (t)
    (display "U[t = ") (display t) (newline)    
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
              (let ((new (generate-symbol v ":" n)))
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

(put 'LAMBDA 'coretok 'true)
(put 'QUOTE 'coretok 'true)
(put 'LET 'mactok 'true)
(put 'IF 'mactok 'true)
(put 'OR 'mactok 'true)
(put 'NAIVE-OR 'mactok 'true)
(put 'FAKE 'mactok 'true)
(put 'CASE 'mactok 'true)

;; macro if expression starts with 
(define macro?
  (lambda (expr)
    (if (pair? expr)
        (memv (car expr) '(LET IF OR NAIVE-OR FAKE CASE))
        #f)))



(define ST
  (lambda (expr)
    (let ((op (car expr)))
      (cond
        ((eq? op 'LET) (let ((i (cadr expr))
                             (e (caddr expr))
                             (b (cadddr expr)))
                         `((LAMBDA ,i ,b) ,e)))
        ((eq? op 'IF) (let ((a (cadr expr))
                            (b (caddr expr))
                            (c (cadddr expr)))
                        `(((ef ,a) ,b) ,c)))
        ((eq? op 'OR) (let ((a (cadr expr))
                            (b (caddr expr)))
                        `(LET v ,a (IF v v ,b))))
        ((eq? op 'NAIVE-OR) (let ((a (cadr expr))
                                  (b (caddr expr)))
                              (let ((v (S-naught 'v)))
                                `(LET ,v ,a (IF ,v ,v ,b)))))
        ((eq? op 'FAKE) (let ((x (cadr expr)))
                          `(QUOTE ,x)))
        ((eq? op 'CASE) (let ((exp (cadr expr))
                              (pr  (caddr expr)))
                          `(LET v ,exp
                                (IF ((eq? v) (quote ,(car pr)))
                                    ,(cadr pr)
                                    'false))))
        (else (error "syntax table: no match" expr))))))






((Ehyg '(LET x (OR a v) (NAIVE-OR x v))) ST)

;;((Ehyg '(LAMBDA a (CASE (FAKE a) (QUOTE a)))) ST)














