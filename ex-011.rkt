#lang htdp/bsl

;; Define a function that consumes two numbers, `x` and `y`, and that computes the
;; distance of point (x, y) to the origin.
(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))


;; Tests
(= (distance-to-origin 3 4)   5)
(= (distance-to-origin 12 5) 13)
