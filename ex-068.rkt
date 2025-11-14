#lang htdp/bsl

;;; An alternative to the nested data representation of balls uses four fields to keep
;;; track of the four properties

(define-struct ballf [x y deltax deltay])

;;; Programmers call this a flat representation. Create an instance of `ballf` that has
;;; the same interpretation as `ball1` (show below)

(define-struct ball [location velocity])
(define-struct vel [deltax deltay])

(define ball1
  (make-ball (make-posn 30 40)
             (make-vel -10 5)))


(define ball2
  (make-ballf 30 40 -10 5))
