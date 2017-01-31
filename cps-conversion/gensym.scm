


(define gensym
  (let ((count 0))
    (lambda ()
      (let ((symbol (string->symbol (string-append "k" (number->string count)))))
	(set! count (+ count 1))
	symbol))))


