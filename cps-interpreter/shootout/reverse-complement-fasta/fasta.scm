


;;-------------------------------------------------------------
;;---------------------------------------------------------
(define symbol-complements
  '((a c g t u m r w s y k v h d b n)
   (t g c a a k y w s r m b d h v n)))

(define upper-character-complements
  (map (lambda (code complement)
         (cons (char-upcase (string-ref (symbol->string code) 0))
               (char-upcase (string-ref (symbol->string complement) 0))))
       (car symbol-complements) (cadr symbol-complements)))

(define lower-character-complements
  (map (lambda (code complement)
         (cons (string-ref (symbol->string code) 0)
               (char-upcase (string-ref (symbol->string complement) 0))))
       (car symbol-complements) (cadr symbol-complements)))

(define character-complements
  (append upper-character-complements
          lower-character-complements))

(define (complement-char x)
  (cdr (assoc x character-complements)))

;; ---------------------------------------------------------------


;; 
;; Use these code complements:
;;-----------------------------
;; code  meaning   complement
;;-----------------------------
;; A    A                   T
;; C    C                   G
;; G    G                   C
;; T/U  T                   A
;; M    A or C              K
;; R    A or G              Y
;; W    A or T              W
;; S    C or G              S
;; Y    C or T              R
;; K    G or T              M
;; V    A or C or G         B
;; H    A or C or T         D
;; D    A or G or T         H
;; B    C or G or T         V
;; N    G or A or T or C    N
;;------------------------------- 

;;(char-ci=? #\a #\a)

;;(read-line)

;;----------------------------------------------------------

;; the last item in a list.
(define (last-item xs)
  (cond 
   ((null? xs) (error "it has no last item"))
   ((null? (cdr xs)) (car xs))
   (else (last-item (cdr xs)))))


;; list of characters in reverse order , excludes newline at end
;; so if a line says
;; 1 2 3 4 5
;; 5 4 3 2 1
;;  result  (5 4 3 2 1) . 5 
(define (read-the-line-reversed)
  (letrec ((loop
            (lambda (xs n)
              (let ((ch (read-char (current-input-port))))
                (cond
                 ;; eof-object disgarded -- return what we;ve got so far.
                 ((eof-object? ch)
                  (done xs n))
                 ;; newline disgarded -- again return 
                 ((char=? ch #\newline)
                  (done xs n))
                 ;; otherwise assume its a live strand
                 (else (loop (cons ch xs) (+ n 1)))))))
           (done (lambda (xs n) (cons xs n))))
    (loop '() 0)))


;; --- theres no data if the lenght of characters counted is zero ? get it ?
(define revobj-no-data?
  (lambda (xs)
    (= (cdr xs) 0)))


(define revobj-header?
  (lambda (xs)
    (and (> (cdr xs) 0)
         (char=? (last-item (car xs)) #\> ))))


(define revobj-chars
  (lambda (xs)
    (car xs)))




;;----------------------------------------------------------

(define *print-index* 1)
(define *print-index-max* 60)

;;---------------------------------------------------------

(define-syntax newline!
  (syntax-rules ()
    ((newline!)
     (begin
       (newline)
       (set! *print-index* 1)))))

(define-syntax display-char!
  (syntax-rules ()
    ((display-char! ch)
     (begin
       ;; if printed upto right margin ,
       ;; newline and reset *print-index* to first column
       (if (> *print-index* *print-index-max*)
	   (begin
	     (newline!))
	   #f)
       ;; show the character
       (begin
	 (display ch)
	 (set! *print-index* (+ 1 *print-index*)))))))

  
;;-----------------------------------------------------------------



(define (parse-file foo)
  (with-input-from-file foo
    (lambda ()
      (entry))))

(define (entry)
  (header-or-comment)
  (newline))




(define (header-or-comment)
  (let ((rd (read-the-line-reversed)))
    (cond
     ((revobj-no-data? rd)
      (flush-output)
      'done)
     ((revobj-header? rd)
      (found-dna-header rd))
     (else
      (error "what now ? ")))))



;;-----------------------------------------
(define (found-dna-header rd)
  ;; header
  (map (lambda (ch) (display ch) ) (reverse(car rd)))
  (newline!)
  (process-the-dna-strand '()))



(define (process-the-dna-strand xs)
  (let ((rd (read-the-line-reversed)))
    (cond
     ((revobj-no-data? rd) ;; now reverse the dna found
      (dump-dna-lists xs))
     ((revobj-header? rd)
      (dump-dna-lists xs)
      (if (> *print-index* 1)
	  (begin (newline!))
	  (begin #f))
      ;; start of new dna strand
      (found-dna-header rd)
      (process-the-dna-strand '()))
     (else
      ;; strand continue
      (process-the-dna-strand (cons (map complement-char (revobj-chars rd)) xs))))))






(define (dump-single-dna-list xs)
  (cond
   ((null? xs) #f)
   (else
    (display-char! (car xs))
    (dump-single-dna-list (cdr xs)))))    


(define (dump-dna-lists xs)
  (cond
   ((null? xs) #f)
   (else
    (dump-single-dna-list (car xs))
    (dump-dna-lists (cdr xs)))))





(define (demo)
  (parse-file "/home/terry/lisp/cps-interpreter/shootout/reverse-complement-fasta/input/revcomp-input.txt"))


;;(demo)
;;(entry)



