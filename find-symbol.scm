
(define (find-symbol id tree)
  (define (find tree)
    (if (pair? tree)
	(or (find (car tree))
	    (find (cdr tree)))
	(if (eq? tree id) (throw 'find #t) #f)))
  (catch 'find (find tree)))

(find-symbol 'foo '(((a . b) . (foo . c)) . (d . e)))


