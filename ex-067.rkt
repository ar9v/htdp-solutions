#lang htdp/bsl

;; Here is another way to represent bouncing balls
(define SPEED 3)
(define-struct balld [location direction])
(make-balld 10 "up")

;; Interpret this code fragment and create other instances of `balld`

;;; A:
;;;
;;; What this is saying is that the ball has a location (could be pixels from the top
;;; or the bottom) and a direction, which is a String denoting where the ball is going.
;;; In this representation, where the ball goes is represented more intuitively, at the
;;; cost of having to deal with the speed separately.

(make-balld 20 "down")
(make-balld 5 "up")
