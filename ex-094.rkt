#lang htdp/bsl

(require 2htdp/image)

;;; Draw some sketches of what the game scenery looks like at
;;; various stages. Use the sketches to determine the constant and the variable
;;; pieces of the game. For the former, develop physical and graphical constants
;;; that describe the dimensions of the world (canvas) and its objects.
;;; Also develop some background scenery.
;;;
;;; Finally, create your initial scene from the constants for the tank, the UFO, and the
;;; background.

; Physical Constants
(define TANK-WIDTH 40)
(define TANK-HEIGHT (/ TANK-WIDTH 3))

(define WORLD-WIDTH (* TANK-WIDTH 10))
(define WORLD-HEIGHT (* TANK-HEIGHT 30))

(define TANK-Y (- WORLD-HEIGHT (/ TANK-HEIGHT 2)))

(define UFO-RADIUS (/ TANK-WIDTH 3))
(define UFO-WING-WIDTH (* TANK-WIDTH 2))
(define UFO-WING-HEIGHT (/ TANK-HEIGHT 5))

; Graphical Constants
(define TANK-COLOR "darkgreen")
(define UFO-BODY-COLOR "midnightblue")
(define UFO-WING-COLOR "grey")

(define UFO
  (overlay (rectangle UFO-WING-WIDTH UFO-WING-HEIGHT "solid" UFO-WING-COLOR)
           (circle UFO-RADIUS "solid" UFO-BODY-COLOR)))

(define TANK
  (rectangle TANK-WIDTH TANK-HEIGHT "solid" TANK-COLOR))

(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT))

(define INITIAL-SCENE
  (place-image
   UFO
   (/ WORLD-WIDTH 2) (image-height UFO)
   (place-image
    TANK
    (/ WORLD-WIDTH 2) TANK-Y
    BACKGROUND)))

INITIAL-SCENE
