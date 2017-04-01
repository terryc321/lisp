


(define fac (lambda (n) (if (= n 1) 1 (* n (fac (- n 1))))))

(display "fac defined")
(newline)





;; ;; map is a cps-primitive
;; map
;; ;; but its a structure , we can pull it apart
;; (define map2 map)
;; (define twice (lambda (x) (+ x x)))

;; ;; check that cps-primitives are first class
;; (map twice '(1 2 3 4 5))
;; (map2 twice '(1 2 3 4 5))
;; (eq? map map2)

;; ;; can we make another map that uses the same cps-primitive
;; ;;(define map3 (cps-primitive (cdr map2)))
;; ;;map3








		   


