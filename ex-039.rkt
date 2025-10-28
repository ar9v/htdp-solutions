#lang htdp/bsl

(require 2htdp/image)

;; Good programmers ensure that an image such as CAR can be enlarged or reduced via a
;; single change to a constant definition. We started the development of our car image
;; with a single plain definition:
(define WHEEL-RADIUS 5)

;;; The definition of WHEEL-DISTANCE is base on the wheel's radius. Hence, changing
;;; WHEEL-RADIUS from 5 to 10 doubles the size of the car image. This kind of program
;;; organization is dubbed "single point of control", and good design employs this idea
;;; as much as possible.

;;; Develop your favorite image of an automobile so that WHEEL-RADIUS remains the single
;;; point of control.
(define WHEEL-DISTANCE (* WHEEL-RADIUS 3))
(define BODY-WIDTH (* WHEEL-RADIUS 8))
(define BODY-HEIGHT (* WHEEL-RADIUS 2))
(define ROOF-WIDTH (* WHEEL-RADIUS 4))
(define ROOF-HEIGHT (* WHEEL-RADIUS 1.5))

(define CAR-COLOR "red")
(define BODY (rectangle BODY-WIDTH BODY-HEIGHT "solid" CAR-COLOR))
(define ROOF (rectangle ROOF-WIDTH ROOF-HEIGHT "solid" CAR-COLOR))
(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define SPACE (rectangle WHEEL-DISTANCE WHEEL-RADIUS 0 "white"))
(define WHEELS (beside WHEEL SPACE WHEEL))

(define CAR
  (above ROOF
         (overlay/offset WHEELS 0 (- (/ BODY-HEIGHT 2)) BODY)))
