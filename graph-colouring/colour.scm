
;; graph colour 

(use-modules (ice-9 pretty-print))

(define pp pretty-print)

;; register interference graph RIG
;; symbol = node
;; pair = edge
(define rig
  '(a b c d e f
      (a c)
      (a f)
      (b c)
      (b e)
      (b f)
      (c d)
      (c e)
      (c f)
      (d e)
      (d f)
      (e f)))

;; assuming only unique (node a)
;; not (node a)(node a)(node a) (node b) == 2 nodes 
(define (node-count rig)
  (cond
   ((null? rig) 0)
   ((symbol? (car rig)) (+ 1 (node-count (cdr rig))))   
   (else (node-count (cdr rig)))))

(define node-eq? eq?)
(define node? symbol?)
(define edge? pair?)
  
(define edge-a car) 
(define edge-b cadr) 


(define (edge-count rig n)
  (cond
   ((null? rig) 0)
   (else
    (let ((e (car rig)))
      (if 
       (and (edge? e)
	    (or (node-eq? n (edge-a e))
		(node-eq? n (edge-b e))))
       ;; if edge matches
       (+ 1 (edge-count (cdr rig) n))
       ;; otherwise try others
       (edge-count (cdr rig) n))))))





;; neighbour-count is an alias for edge-count
(define neighbour-count edge-count)


;; optimistic colouring algorithm
;;
;; pick a node t with fewer than k neighbours
;; put on a stack and remove it from the rig
;; repeat until graph has one node
;;



;; ;; k-colourable register interference graph
(define (colour k rig)
  (let ((stack '()))
    (colour-help k rig stack)))


;; ;; list nodes with fewer than k neighbours or k edges??
(define (fook k rig)
  (fook2 k rig rig))

(define (fook2 k rig all-rig)
  (cond
   ((null? rig) '())
   ((and (node? (car rig))
	 (< (neighbour-count all-rig (car rig)) k))
    (cons (car rig)
	  (fook2 k (cdr rig) all-rig)))
   (else
    (fook2 k (cdr rig) all-rig))))



;; if graph has one node or fewer then stop
;; pick a node with edge count fewer than k
(define (colour-help k rig stack)
  (cond
   ((= (node-count rig) 1) (list 'done stack))
   ((not (null? (fook k rig)))
    (let ((sel (car (fook k rig))))
      (colour-help k (remove-node-and-edges sel rig) (cons sel stack))))
   (else (error "colour-help : " k rig stack))))


;;
(define (remove-node-and-edges node rig)
  (cond
   
   ;; no more rig
   ((null? rig) rig)
   
   ;; remove mention of this node   
   ((node-eq? node (car rig))
    (remove-node-and-edges node (cdr rig)))
   
   ;; other nodes - include them
   ((node? (car rig))
    (cons (car rig) (remove-node-and-edges node (cdr rig))))
   
   ;; node matches edge A
   ((node-eq? node (edge-a (car rig)))
    (remove-node-and-edges node (cdr rig)))
   
   ;; node matches edge B   
   ((node-eq? node (edge-b (car rig)))
    (remove-node-and-edges node (cdr rig)))
   
   ;;
   (else 
    (cons (car rig) (remove-node-and-edges node (cdr rig))))))


;; *********** colour graph
;;
;; the RIG for this example
;; (a b c d e f (a c) (a f) (b c) (b e) (b f) (c d) (c e) (c f) (d e) (d f) (e f))
;;
;; (colour 4 rig) -- succeeds 
;; = (done (e d c b a))
;;
;;
;; (colour 3 rig) -- fails as no way to colour the rig
;; 
   
   








  






















    




