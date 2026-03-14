#lang htdp/isl+

(require 2htdp/image)

;;; To compute the endpoints of an equilateral Sierpinski triangle, draw a circle and
;;; pick three points on the circle that are 120 degrees apart, for example, 120, 240, and
;;; 360.
;;;
;;; Design the function `circle-pt`:

(define CENTER (make-posn 200 200))
(define RADIUS 200) ; the radius in pixels

(define MT (empty-scene 500 500))

; circle-pt: Number -> Posn
; determines the point on the circle with CENTER and RADIUS whose angle is `factor`
;
; examples
; what are the x and y coordinates of the desired
; point, when given: 120/360, 240/360, 360/360
(check-within (circle-pt 120/360)
              (make-posn
               (+ (real-part (make-polar RADIUS (* 120/360 2 pi))) (posn-x CENTER))
               (+ (imag-part (make-polar RADIUS (* 120/360 2 pi))) (posn-y CENTER)))
              0.001)
(define (circle-pt factor)
  (local [(define polar (make-polar RADIUS (* factor 2 pi)))
          (define x (+ (real-part polar) (posn-x CENTER)))
          (define y (+ (imag-part polar) (posn-y CENTER)))]
    (make-posn x y)))

;;; We can illustrate `circle-pt` by mapping out the points in an image.
;;;
;;; Since the y-axis is flipped, `a` appears below while `b` appears above
(define A (text "a" 16 'blue))
(define B (text "b" 16 'blue))
(define C (text "c" 16 'blue))
(define CI (place-image (circle RADIUS 'outline 'red) (posn-x CENTER) (posn-y CENTER) MT))

(define a (circle-pt 120/360))
(define b (circle-pt 240/360))
(define c (circle-pt 360/360))

(define ax (posn-x a))
(define ay (posn-y a))

(define bx (posn-x b))
(define by (posn-y b))

(define cx (posn-x c))
(define cy (posn-y c))

(place-image
 A
 ax ay
 (place-image
  B
  bx by
  (place-image
   C
   cx cy
   CI)))
