#lang htdp/isl+

;;; Design the function `distance-between`. It consumes two numbers and a Posn: x, y, and
;;; p. The function computes the distance between the points (x, y) and p

; distance-between: Number Number Posn -> Number
; computes the distance between (`x`, `y`) and `p`
(check-expect (distance-between 3 4 (make-posn 0 0)) 5)
(check-expect (distance-between 5 0 (make-posn 3 0)) 2)
(check-within (distance-between 3 4 (make-posn 0 9)) (sqrt 34) 0.0001)
(define (distance-between x y p)
  (sqrt (+ (sqr (- (posn-x p) x))
           (sqr (- (posn-y p) y)))))
