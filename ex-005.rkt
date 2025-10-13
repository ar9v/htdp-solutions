#lang htdp/bsl

;; Use the `2htdp/image` library to create the image of a simple boat or tree
;; Make sure you can easily change the scale of the entire image

(require 2htdp/image)

(define (isosceles-trapezoid a b h mode color)
  (beside
   (flip-horizontal (right-triangle (/ (abs (- a b)) 2) h mode color))
   (rectangle (min a b) h mode color)
   (right-triangle (/ (abs (- a b)) 2) h mode color)))


;; Physical Constants
(define HULL-HEIGHT 50)
(define HULL-WIDTH (* HULL-HEIGHT 6))
(define MAST-WIDTH (/ HULL-HEIGHT 4))
(define MAST-HEIGHT (* 4 HULL-HEIGHT))
(define SAIL-HEIGHT MAST-HEIGHT)

;; Graphical Constants
(define HULL
  (flip-vertical
   (isosceles-trapezoid (/ HULL-WIDTH 2) HULL-WIDTH HULL-HEIGHT "solid" "brown")))
(define MAST (rectangle MAST-WIDTH MAST-HEIGHT "solid" "brown"))
(define SAIL (triangle SAIL-HEIGHT "solid" "white"))

(define BOAT
  (above (overlay/align "middle" "top" MAST SAIL)
         HULL))

BOAT
