#lang htdp/bsl

;; Add the following definitions for x and y to the definitions area
(define x 3)
(define y 4)

;; Now imagine that x and y are the coordinates of a Cartesian point.
;;
;; Write down an expression that computes the distance of this point to the
;; origin, that is, a point with the coordinates (0, 0).

;; Answer
;; The book calls for an expression `(sqrt (+ (sqr x) (sqr y)))`, but we define
;; a function for convenience.
(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))

;; Tests
(= (distance-to-origin x y)   5)
(= (distance-to-origin 12 5) 13)
