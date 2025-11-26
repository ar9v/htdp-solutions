#lang htdp/bsl

(require 2htdp/image)

;;; Some program contains the following data definition:

; A Coordinate is one of:
; -- a NegativeNumber
; interpretation: on the y axis, distance from top
; -- a PositiveNumber
; interpretation: on the x axis, distance from left
; -- a Posn
; interpretation: an ordinary Cartesian point

;;; Make up at least two data examples per clause in the data definition.
;;; For each of the examples explain its meaning with a sketch of a canvas.

(define SCN (empty-scene 200 200))

(define negative-ex-1 -50)
(place-image/align (rectangle 300 1 "solid" "red")
                   0 50
                   "left" "top"
                   SCN)

(define negative-ex-2 -30)
(place-image/align (rectangle 300 1 "solid" "red")
                   0 30
                   "left" "top"
                   SCN)

(define positive-ex-1 70)
(place-image/align (rectangle 1 300 "solid" "red")
                   70 0
                   "left" "top"
                   SCN)

(define positive-ex-2 150)
(place-image/align (rectangle 1 300 "solid" "red")
                   150 0
                   "left" "top"
                   SCN)

(define posn-ex-1 (make-posn 20 80))
(place-image (circle 5 "solid" "red")
             20 80
             SCN)

(define posn-ex-2 (make-posn 70 60))
(place-image (circle 5 "solid" "red")
             70 60
             SCN)
