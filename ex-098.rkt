#lang htdp/bsl

(require 2htdp/image)

;;; Design the function `si-game-over?` for use as the `stop-when` handler. The game stops
;;; if the UFO lands or if the missile hits the UFO. For both conditions, we recommend
;;; that you check for proximity of one object to another.
;;;
;;; The `stop-when` clause allows for an optional second sub-expression, namely a function
;;; that renders the final state of the game. Design `si-render-final` and use it as the
;;; second part for your `stop-when` clause in the `main` function of exercise 100.

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
(define GAME-OVER-FONT-COLOR "black")
(define GAME-WON-FONT-COLOR "green")
(define FONT-SIZE 30)

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
(define landed-while-aiming
  (make-aim (make-posn 50 (- WORLD-HEIGHT UFO-RADIUS))
            (make-tank 20 5)))
(define landed-after-firing
  (make-fired (make-posn 50 (- WORLD-HEIGHT UFO-RADIUS))
              (make-tank 150 -3)
              (make-posn 150 10)))

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

; si-game-over?: SIGS -> Boolean
; Returns true if the UFO has landed or if it has been hit by the missile.
(check-expect (si-game-over? aiming) #false)
(check-expect (si-game-over? landed-while-aiming) #true)
(check-expect (si-game-over? just-fired) #false)
(check-expect (si-game-over? landed-after-firing) #true)
(check-expect (si-game-over? about-to-hit) #true)
(define (si-game-over? s)
  (cond [(aim? s) (landed? (aim-ufo s))]
        [(fired? s) (or (hit-by-missile? (fired-ufo s) (fired-missile s))
                        (landed? (fired-ufo s)))]))

; hit-by-missile?: UFO Missile -> Boolean
; returns true if `ufo` and `missile` have `collided?`
(check-expect (hit-by-missile? (fired-ufo just-fired) (fired-missile just-fired))
              #false)
(check-expect (hit-by-missile? (fired-ufo about-to-hit) (fired-missile about-to-hit))
              #true)
(define (hit-by-missile? ufo m)
  (<= (distance ufo m) UFO-RADIUS))

; landed?: UFO -> Boolean
; returns true if `ufo` has reached the bottom of the BACKGROUND
(check-expect (landed? (aim-ufo aiming)) #false)
(check-expect (landed? (aim-ufo landed-while-aiming)) #true)
(check-expect (landed? (fired-ufo about-to-hit)) #false)
(check-expect (landed? (fired-ufo just-fired)) #false)
(check-expect (landed? (fired-ufo landed-after-firing)) #true)
(define (landed? ufo)
  (<= (- WORLD-HEIGHT UFO-RADIUS)
      (posn-y ufo)))

; distance: Posn Posn -> Number
; Given p1 and p2, return the distance between them
(check-expect (distance (make-posn 0 0) (make-posn 5 0)) 5)
(check-expect (distance (make-posn 0 0) (make-posn 0 5)) 5)
(check-expect (distance (make-posn 1 1) (make-posn 2 2))
              (inexact->exact (sqrt (+ (sqr (- 2 1))
                                       (sqr (- 2 1))))))
(check-expect (distance (make-posn 20 100) (make-posn 22 103))
              (inexact->exact (sqrt (+ (sqr (- 22 20))
                                       (sqr (- 103 100))))))
(define (distance p1 p2)
  (inexact->exact (sqrt (+ (sqr (- (posn-x p2) (posn-x p1)))
                           (sqr (- (posn-y p2) (posn-y p1)))))))

; si-render-final: SIGS -> Image
; Render the final state of the game, with text describing the outcome
(check-expect (si-render-final landed-after-firing)
              (overlay/align
               "center" "top"
               (text "Game Over" FONT-SIZE GAME-OVER-FONT-COLOR)
               (si-render landed-after-firing)))
(check-expect (si-render-final about-to-hit)
              (overlay/align
               "center" "top"
               (text "You Win!" FONT-SIZE GAME-WON-FONT-COLOR)
               (si-render about-to-hit)))
(define (si-render-final s)
  (overlay/align
   "center" "top"
   (cond [(or (aim? s) (landed? (fired-ufo s)))
          (text "Game Over" FONT-SIZE GAME-OVER-FONT-COLOR)]
         [else
          (text "You Win!" FONT-SIZE GAME-WON-FONT-COLOR)])
   (si-render s)))
