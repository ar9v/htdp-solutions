#lang htdp/bsl

(require 2htdp/image)

;;; Design the functions `tank-render`, `ufo-render`, and `missile-render`.

; Physical Constants
(define TANK-WIDTH 40)
(define TANK-HEIGHT (/ TANK-WIDTH 3))

(define WORLD-WIDTH (* TANK-WIDTH 10))
(define WORLD-HEIGHT (* TANK-HEIGHT 30))

(define TANK-Y (- WORLD-HEIGHT (/ TANK-HEIGHT 2)))

(define MISSILE-SIZE (/ TANK-WIDTH 2))

(define UFO-RADIUS (/ TANK-WIDTH 3))
(define UFO-WING-WIDTH (* TANK-WIDTH 2))
(define UFO-WING-HEIGHT (/ TANK-HEIGHT 5))

; Graphical Constants
(define TANK-COLOR "darkgreen")
(define MISSILE-COLOR "brown")
(define UFO-BODY-COLOR "midnightblue")
(define UFO-WING-COLOR "grey")

(define UFO
  (overlay (rectangle UFO-WING-WIDTH UFO-WING-HEIGHT "solid" UFO-WING-COLOR)
           (circle UFO-RADIUS "solid" UFO-BODY-COLOR)))

(define TANK
  (rectangle TANK-WIDTH TANK-HEIGHT "solid" TANK-COLOR))

(define MISSILE
  (triangle MISSILE-SIZE "solid" MISSILE-COLOR))

(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT))

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A UFO is a Posn
; interpretation: (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number)
;
; interpretation: (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missile is a Posn
; interpretation: (make-posn x y) is the missile's place

; A SIGS is one of:
; -- (make-aim UFO Tank)
; -- (make-fired UFO Tank Missile)
;
; interpretation: represents the complete state of a space invader game

(define aiming (make-aim (make-posn 20 10) (make-tank 28 -3)))
(define about-to-hit
  (make-fired (make-posn 20 100)
              (make-tank 100 3)
              (make-posn 22 103)))
(define just-fired
  (make-fired (make-posn 20 100)
              (make-tank 100 3)
              (make-posn 22 (- WORLD-HEIGHT TANK-HEIGHT MISSILE-SIZE))))

; si-render: SIGS -> Image
; adds TANK, UFO, and possibly MISSILE to the BACKGROUND scene
(define (si-render s)
  (cond [(aim? s)
         (tank-render (aim-tank s)
                      (ufo-render (aim-ufo s) BACKGROUND))]
        [(fired? s)
         (tank-render
          (fired-tank s)
          (ufo-render (fired-ufo s)
                      (missile-render (fired-missile s)
                                      BACKGROUND)))]))

; tank-render: Tank Image -> Image
; adds t to the given image im
(check-expect (tank-render (make-tank 50 5) BACKGROUND)
              (place-image TANK 50 TANK-Y BACKGROUND))
(define (tank-render t im)
  (place-image TANK (tank-loc t) TANK-Y im))

; ufo-render: UFO Image -> Image
; adds u to the given image im
(check-expect (ufo-render (make-posn 50 70) BACKGROUND)
              (place-image UFO 50 70 BACKGROUND))
(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; missile-render: Missile Image -> Image
; adds m to the given image im
(check-expect (missile-render (make-posn 100 150) BACKGROUND)
              (place-image MISSILE 100 150 BACKGROUND))
(define (missile-render m im)
  (place-image MISSILE (posn-x m) (posn-y m) im))


;;; Compare these expressions:

;; (tank-render
;;  (fired-tank s)
;;  (ufo-render (fired-ufo s)
;;              (missile-render (fired-missile s)
;;                              BACKGROUND)))

;; (ufo-render
;;  (fired-ufo s)
;;  (tank-render (fired-tank s)
;;               (missile-render (fired-missile s)
;;                               BACKGROUND)))

;;; When do the two expressions produce the same result?

;;; A: Always; order does not matter (much like adding numbers)
