;; when
(defmacro mywhen (test &body body)
  `(if ,test
       (progn
	 ,@body)))

(defmacro mymac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro myfor-bad ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
	(limit ,stop))
       ((> ,var limit))
     ,@body))

;;
(myfor-bad (a 1 10) (format t "a has value ~a ~%" a))
;; macro arg capture
(myfor-bad (var 1 10) (format t "var has value ~a ~%" var))
(myfor-bad (start 1 10) (format t "var has value ~a ~%" start))

;; lets see expansion
(mymac (myfor-bad (a 1 10) (format t "a has value ~a ~%" a)))
;; (DO ((A 1 (1+ A))
;;      (LIMIT 10))
;;     ((> A LIMIT))
;;   (FORMAT T "a has value ~a ~%" A))

;; (mymac (myfor-bad (limit 1 10) (format t "limit has value ~a ~%" limit)))
;; (DO ((LIMIT 1 (1+ LIMIT))
;;      (LIMIT 10))
;;     ((> LIMIT LIMIT))
;;   (FORMAT T "limit has value ~a ~%" LIMIT))


;; (mymac (myfor-bad (a 1 10) (format t "limit has value ~a ~%" limit)))
;; (DO ((LIMIT 1 (1+ LIMIT))
;;      (LIMIT 10))
;;     ((> LIMIT LIMIT))
;;   (FORMAT T "limit has value ~a ~%" LIMIT))

;;(mymac (memq 'a '(a b c)))


(defvar w nil)

(defmacro gripe (warning)
  `(progn (setq w (nconc w (list ,warning)))
	  nil))

(defun bad-sample-ratio(v w)
  (let ((vn (length v))
	(wn (length w)))
    (if (or (< vn 2) (< wn 2))
	(gripe "sample < 2")
	(/ vn wn))))


;;(mymac (gripe "sample < 2"))
;;(PROGN (SETQ W (NCONC W (LIST "sample < 2"))) NIL)

;; bad-sample-ratio has variable w , captured in gripe macro

(defun check-sample-ratio (a b)
  (format t "global w is ~a ~%" w)
  (bad-sample-ratio a b))

;; not noticeable 
(check-sample-ratio '(1 2 3) '(4 5 6))
(check-sample-ratio '(1 2 3 4 5 6) '(4 5 6))
(check-sample-ratio '(1) '(4 5 6))
(check-sample-ratio '(1) '(4 5 6))
;; let variable
(defun check ()
  (let ((temp '(b)))
    (progn
      (bad-sample-ratio '(4 5 6) temp)
      (format t "temp has value ~a ~%" temp)
      (format t "w has value ~a ~%" w)))
  )
;; just alters temp , rather than '(4 5 6)


;; ----- infinite loop , limit side effected and
;;
;; (defmacro myfor-bad ((var start stop) &body body)
;;   `(do ((,var ,start (1+ ,var))
;; 	(limit ,stop))
;;        ((> ,var limit))
;;      ,@body))
;;
;; 1 ) limit is FREE VARIABLE , any change to limit in scope of this macro usage DESTROYS the purpose.
;; 2 ) if var=limit as (myfor-bad limit 0 10) then (> limit limit) 
;;
;; ------
;; (let ((limit 100))
;;   (myfor-bad (po 1 10)
;;     (incf limit)
;;     (format t "limit has value ~a ~%" limit))
;;   (format t "original limit has value ~a ~%" limit))
;; ---------
;;
;; 










