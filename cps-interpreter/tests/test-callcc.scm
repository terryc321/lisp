

;; this is plain weird
;; as it captures continuation of
;; it loops once , then crashes , explain.

(define k1 (call-with-current-continuation (lambda (k) k)))
(format #t "k1 = ~a ~%" k1)
;;(begin 1 2 3 (k1 4) 5 6 7)
k1
(format #t "k1 = ~a ~%" k1)

(define k1 #f)
(set! k1 (call-with-current-continuation (lambda (k) k)))
(format #t "k1.loop = ~a ~%" k1)
(k1 'x)









