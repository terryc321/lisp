

;; assoc
(define (assoc symbol alist) 
  (cond
   ((null? alist) '())
   ((eq? symbol (car (car alist)))
    (car alist))
   (else (assoc symbol (cdr alist)))))


;; tack (symbol . value) onto front of alist
(define (acons symbol value alist)
  (cons (cons symbol value) alist))


(assoc 'a '((a 1)(b 2)(c 3)(d 4)))
(assoc 'b '((a 1)(b 2)(c 3)(d 4)))
(assoc 'd '((a 1)(b 2)(c 3)(d 4)))
(assoc 'f '((a 1)(b 2)(c 3)(d 4)))
(assoc 'f '((a 1)(b 2)(c 3)(d 4)))

(assoc 'does-not-exist '((a 1)(b 2)(c 3)(d 4)))

(assoc 'does-not-exist (acons 'does-not-exist 'it-does-now '((a 1)(b 2)(c 3)(d 4))))



