
;;solution is due to  http://stackoverflow.com/users/9453/daniel-dipaolo
;;http://stackoverflow.com/questions/2595132/how-to-solve-n-queens-in-scheme
;;


(define (add1 n) (1+ n))
(define (sub1 n) (1- n))


(define (length xs)
  (cond
   ((null? xs) 0)
   (else (+ 1 (length (cdr xs))))))


;_____________________________________________________
;This function tests to see if the next attempted move (try)
;is legal, given the list that has been constructed thus far
;(if any) - legal-pl (LEGAL PLacement list)
;N.B. - this function is an EXACT copy of the one from
;Springer and Friedman

(define good?
  (lambda (new-pl up down)
    (cond
     ((null? new-pl) #t)
     (else (let ((next-pos (car new-pl)))
	     (and
	      (not (= next-pos try))
	      (not (= next-pos up))
	      (not (= next-pos down))
	      (good? (cdr new-pl)
		     (add1 up)
		     (sub1 down))))))))


(define legal?
  (lambda (try legal-pl)
    (good? legal-pl (add1 try) (sub1 try))))



;; we dont do LETREC yet
;;
;; (define legal?
;;   (lambda (try legal-pl)
;;     (letrec
;;         ((good?
;;           (lambda (new-pl up down)
;;             (cond
;;               ((null? new-pl) #t)
;;               (else (let ((next-pos (car new-pl)))
;;                       (and
;;                        (not (= next-pos try))
;;                        (not (= next-pos up))
;;                        (not (= next-pos down))
;;                        (good? (cdr new-pl)
;;                               (add1 up)
;;                               (sub1 down)))))))))
;;       (good? legal-pl (add1 try) (sub1 try)))))



;;_____________________________________________________
;This function tests the length of the solution to
;see if we need to continue "cons"ing on more terms
;or not given to the specified board size.
;
;I modified this function so that it could test the
;validity of any solution for a given boardsize.
(define solution?
    (lambda (legal-pl boardsize)
      (= (length legal-pl) boardsize)))
;_____________________________________________________
;I had to modify this function so that it was passed
;the boardsize in its call, but other than that (and
;simply replacing "fresh-start" with boardsize), just
;about no changes were made.  This function simply
;generates a solution.
(define build-solution
  (lambda (legal-pl boardsize)
    (cond
      ((solution? legal-pl boardsize) legal-pl)
      (else (forward boardsize legal-pl boardsize)))))
;_____________________________________________________
;This function dictates how the next solution will be
;chosen, as it is only called when the last solution
;was proven to be legal, and we are ready to try a new
;placement.
;
;I had to modify this function to include the boardsize
;as well, since it invokes "build-solution".
(define forward
  (lambda (try legal-pl boardsize)
    (cond
      ((zero? try) (backtrack legal-pl boardsize))
      ((legal? try legal-pl) (build-solution (cons try legal-pl) boardsize))
      (else (forward (sub1 try) legal-pl boardsize)))))
;_____________________________________________________
;This function is used when the last move is found to
;be unhelpful (although valid) - instead it tries another
;one until it finds a new solution.
;
;Again, I had to modify this function to include boardsize
;since it calls "forward", which has boardsize as a
;parameter due to the "build-solution" call within it
(define backtrack
  (lambda (legal-pl boardsize)
    (cond
      ((null? legal-pl) '())
      (else (forward (sub1 (car legal-pl)) (cdr legal-pl) boardsize)))))
;_____________________________________________________
;This is pretty much the same function as the one in the book
;with just my minor "boardsize" tweaks, since build-solution
					;is called.

(define build-loop
  (lambda (boardsize sol)
    (cond
     ((null? sol) '())
     (else (cons sol (build-loop boardsize (backtrack sol boardsize)))))))


(define build-all-solutions
  (lambda (boardsize)
    (build-loop boardsize (build-solution '() boardsize))))


;_____________________________________________________
;This function I made up entirely myself, and I only
;made it really to satisfy the syntactical limitations
;of the laboratory instructions.  This makes it so that
;the input of "(queens 4)" will return a list of the
;two possible configurations that are valid solutions,
;even though my modifiend functions would return the same
;value by simply inputting "(build-all-solutions 4)".
(define queens
  (lambda (n)
    (build-all-solutions n)))



