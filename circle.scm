
;; circle area pi r r 
(define (circle-area radius)
  (* pi radius radius))

;; sphere area 4 pi r r
;; 4 circle-area
(define (sphere-area radius)
  (* 4 (circle-area radius)))

;; sphere volume ( 4 / 3 ) pi r r r
;; circle-area * r / 3 
(define (sphere-volume radius)
  (/ (* radius (sphere-area radius)) 3))

(define pi 3.1415926535898)




