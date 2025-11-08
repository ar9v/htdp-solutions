#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; The design recipe for world programs demands that you translate information into data
;; and vice versa to ensure a complete understanding of the data definition. It's best
;; to draw some world scenarios and to represent them with data and, conversely, to pick
;; some data examples and to draw pictures that match them. Do so for the LR definition,
;; including at least HEIGHT and 0 as examples.

; Data Definition
;
; An LR (short for launching rocket) is one of:
; -- "resting"
; -- NonnegativeNumber
; interpretation: "resting" represents a grounded rocket
; a number denotes the height of a rocket in flight
;
; interpretation: NonnegativeNumber is the distance between the top of the canvas and
; the reference point (in this case, the center of the rocket)

;;; Given the following
(define WIDTH 100)
(define HEIGHT 200)
(define X-CENTER (/ WIDTH 2))

(define SCENE (empty-scene WIDTH HEIGHT))

(define ROCKET (bitmap/file "images/rocket.png"))
(define CENTER (/ (image-height ROCKET) 2))

;;; Info -> Data

;;; When the launching rocket is "resting", its position is
;;; (X-CENTER, HEIGHT)
(place-image ROCKET X-CENTER HEIGHT SCENE)

;;; When the launching rocket is 0, that means that it is positioned at
;;; (X-CENTER, 0), with the point of reference being the rocket's center
;;;
;;; Since the point of reference is the ROCKET's center, we only see the lower half
(place-image ROCKET X-CENTER 0 SCENE)

;;; When the launching rocket is 50, that means that it is positioned at
;;; (X-CENTER, 50).
(place-image ROCKET X-CENTER 50 SCENE)

;;; When the launching rocket is HEIGHT, that means that it is positioned at
;;; (X-CENTER, HEIGHT).
;;;
;;; Since the point of reference is the rocket's center, this technically means that
;;; we only see the ROCKET's upper half
(place-image ROCKET X-CENTER HEIGHT SCENE)

;;; Data -> Info

;;; "resting" -> The rocket is grounded

;;; 0 -> The rocket is at the top of the canvas position (with only it's lower half
;;; rendering)

;;; 50 -> The rocket is 50 pixels away from reaching the top of the canvas, or the
;;; topmost visible part.

;;; HEIGHT -> The rocket is rendered at the bottom of the canvas, which is the lowermost
;;; visible part of the image for us. Only the upper half of it can be seen, since the
;;; reference is the rocket's center.

;;; NOTE:
;;; We've taken the interpretation at face value, which means we may render states that
;;; don't look great. We could consistently render images by subtracting CENTER from
;;; the current state, such that a grounded rocket would be the image of the rocket when
;;; it's bottom border touches the bottom of the canvas.
