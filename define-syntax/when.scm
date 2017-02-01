

;; our famous WHEN macro 

(define-syntax when
  (syntax-rules ()
    ((when condition body ...)
     (if condition (begin body ...) #f))))



