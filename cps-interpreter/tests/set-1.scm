



(define a 1)
(define b 'a)

(set b 123 (current-environment))

;; expect a has value 123
a

(list
 (let ((a 2)(b 'a))
   (set b 456 (current-environment))
   a)
 a)

;; expect 456 123








