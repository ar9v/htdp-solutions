#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Design the function `si-control`, which plays the role of the key-event handler. As
;;; such, it consumes a game state and a KeyEvent and produces a new game state.
;;;
;;; It reacts to three different keys:
;;;
;;; - pressing the left arrow ensures the tank moves left;
;;; - pressing the right arrow ensures the tank moves right; and
;;; - pressing the space bar fires the missile if it hasn't been launched yet.
;;;
;;; Once you have this function, you can define the `si-main` function,
;;; which uses `big-bang` to spawn the game-playing window. Enjoy!

; Physical Constants
(define TANK-WIDTH 40)
(define TANK-HEIGHT (/ TANK-WIDTH 3))

(define WORLD-WIDTH (* TANK-WIDTH 10))
(define WORLD-HEIGHT (* TANK-HEIGHT 30))

(define TANK-Y (- WORLD-HEIGHT (/ TANK-HEIGHT 2)))

(define UFO-RADIUS (/ TANK-WIDTH 3))
(define UFO-PADDING (* TANK-WIDTH 2))
(define UFO-WING-WIDTH (* TANK-WIDTH 2))
(define UFO-WING-HEIGHT (/ TANK-HEIGHT 5))
(define UFO-RATE-OF-DESCENT 2)

(define MISSILE-SIZE (/ TANK-WIDTH 2))
(define MISSILE-SPEED (* UFO-RATE-OF-DESCENT 2))

(define CLOCK-RATE 1/28)

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
(define left-bound (make-tank (sub1 (/ TANK-WIDTH 2)) (- TANK-WIDTH)))
(define right-bound (make-tank (add1 (- WORLD-WIDTH (/ TANK-WIDTH 2))) TANK-WIDTH))

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

; si-main: SIGS -> SIGS
; Runs the game
(define (si-main s)
  (big-bang s
            [to-draw si-render]
            [on-tick si-move CLOCK-RATE]
            [on-key si-control]
            [stop-when si-game-over? si-render-final]
            [check-with sigs?]))

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

; si-move: SIGS -> SIGS
; Determines the next SIGS state
(define (si-move s)
  (cond [(aim? s)
         (make-aim (move-ufo (aim-ufo s))
                   (move-tank (aim-tank s)))]
        [(fired? s)
         (make-fired (move-ufo (fired-ufo s))
                     (move-tank (fired-tank s))
                     (move-missile (fired-missile s)))]))

; move-ufo: UFO -> UFO
; Spawn `ufo` in a random place along the x axis, UFO-RATE-OF-DESCENT units lower on
; the y axis. The random place along the x axis is within
; [(UFO-WING-WIDTH + UFO-PADDING)/2, WORLD-WIDTH - UFO-WING-WIDTH - UFO-PADDING],
; so the UFO is always displayed fully within the canvas.
(check-random (move-ufo (make-posn 50 50))
              (make-posn (+ (random (- WORLD-WIDTH UFO-WING-WIDTH UFO-PADDING))
                            (/ (+ UFO-WING-WIDTH UFO-PADDING) 2))
                         (+ 50 UFO-RATE-OF-DESCENT)))
(define (move-ufo ufo)
  (make-posn (+ (random (- WORLD-WIDTH UFO-WING-WIDTH UFO-PADDING))
                (/ (+ UFO-WING-WIDTH UFO-PADDING) 2))
             (+ (posn-y ufo) UFO-RATE-OF-DESCENT)))

; move-tank: Tank -> Tank
; Move `t`a ccording to its `vel`.
; If its new position exceeds 0 + TANK-WIDTH/2 or WORLD-WIDTH - TANK-WIDTH/2, place it in
; TANK-WIDTH/2 or WORLD-WIDTH - TANK-WIDTH/2 respectively
(check-expect (move-tank (aim-tank aiming))
              (make-tank (+ (tank-loc (aim-tank aiming))
                            (tank-vel (aim-tank aiming)))
                         (tank-vel (aim-tank aiming))))
(check-expect (move-tank left-bound) (make-tank (/ TANK-WIDTH 2) (- TANK-WIDTH)))
(check-expect (move-tank right-bound) (make-tank (- WORLD-WIDTH (/ TANK-WIDTH 2))
                                                 TANK-WIDTH))
(define (move-tank t)
  (make-tank
   (cond [(positive? (tank-vel t))
          (min (- WORLD-WIDTH (/ TANK-WIDTH 2)) (+ (tank-loc t) (tank-vel t)))]
         [(negative? (tank-vel t))
          (max (/ TANK-WIDTH 2) (+ (tank-loc t) (tank-vel t)))])
   (tank-vel t)))

; move-missile: Missile -> Missile
; Moves the missile upwards in a straight line
(check-expect (move-missile (make-posn 50 150))
              (make-posn 50 (- 150 MISSILE-SPEED)))
(define (move-missile m)
  (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED)))

; si-control: SIGS KeyEvent -> SIGS
; Produces a new game event according to the following:
; - pressing the left arrow ensures the tank moves left;
; - pressing the right arrow ensures the tank moves right; and
; - pressing the space bar fires the missile if it hasn't been launched yet.
(check-expect (si-control aiming "left") (sigs-tank-left aiming))
(check-expect (si-control aiming "right") (sigs-tank-right aiming))
(check-expect (si-control aiming " ")
              (make-fired (aim-ufo aiming) (aim-tank aiming) (fire! (aim-tank aiming))))
(check-expect (si-control aiming "a") aiming)

(check-expect (si-control just-fired "left") (sigs-tank-left just-fired))
(check-expect (si-control just-fired "right") (sigs-tank-right just-fired))
(check-expect (si-control just-fired " ") just-fired)
(check-expect (si-control just-fired "a") just-fired)
(define (si-control s ke)
  (cond [(key=? ke "left") (sigs-tank-left s)]
        [(key=? ke "right") (sigs-tank-right s)]
        [(and (key=? ke " ") (aim? s))
         (make-fired (aim-ufo s) (aim-tank s) (fire! (aim-tank s)))]
        [else s]))

; sigs-tank-left: SIGS -> SIGS
; Shifts SIGS `s`'s tank velocity sign to a negative sign
(check-expect (sigs-tank-left aiming) aiming)
(check-expect (sigs-tank-left (make-aim (make-posn 50 50) (make-tank 100 3)))
              (make-aim (make-posn 50 50) (make-tank 100 -3)))
(check-expect (sigs-tank-left just-fired)
              (make-fired (fired-ufo just-fired)
                          (make-tank 100 -3)
                          (fired-missile just-fired)))
(check-expect (sigs-tank-left (make-fired (make-posn 50 50)
                                          (make-tank 100 -3)
                                          (make-posn 100 100)))
              (make-fired (make-posn 50 50)
                          (make-tank 100 -3)
                          (make-posn 100 100)))
(define (sigs-tank-left s)
  (cond [(aim? s)
         (make-aim (aim-ufo s) (tank-vel-left (aim-tank s)))]
        [(fired? s)
         (make-fired (fired-ufo s)
                     (tank-vel-left (fired-tank s))
                     (fired-missile s))]))

; sigs-tank-right: SIGS -> SIGS
; Shifts SIGS `s`'s tank velocity sign to a positive sign
(check-expect (sigs-tank-right aiming) (make-aim (make-posn 20 10) (make-tank 28 3)))
(check-expect (sigs-tank-right (make-aim (make-posn 50 50) (make-tank 100 3)))
              (make-aim (make-posn 50 50) (make-tank 100 3)))
(check-expect (sigs-tank-right just-fired) just-fired)
(check-expect (sigs-tank-right (make-fired (make-posn 50 50)
                                          (make-tank 100 -3)
                                          (make-posn 100 100)))
              (make-fired (make-posn 50 50)
                          (make-tank 100 3)
                          (make-posn 100 100)))
(define (sigs-tank-right s)
  (cond [(aim? s)
         (make-aim (aim-ufo s) (tank-vel-right (aim-tank s)))]
        [(fired? s)
         (make-fired (fired-ufo s)
                     (tank-vel-right (fired-tank s))
                     (fired-missile s))]))

; tank-vel-left: Tank -> Tank
; Shifts `t`'s vel to a negative sign
(check-expect (tank-vel-left (make-tank 28 3)) (make-tank 28 -3))
(check-expect (tank-vel-left (make-tank 28 -3)) (make-tank 28 -3))
(define (tank-vel-left t)
  (make-tank (tank-loc t) (* (abs (tank-vel t)) -1)))

; tank-vel-right: Tank -> Tank
; Shifts `t`'s vel to a positive sign
(check-expect (tank-vel-right (make-tank 28 3)) (make-tank 28 3))
(check-expect (tank-vel-right (make-tank 28 -3)) (make-tank 28 3))
(define (tank-vel-right t)
  (make-tank (tank-loc t) (abs (tank-vel t))))

; fire!: Tank -> Missile
; Produces a Missile with starting coordinates ((tank-vel t), WORLD-HEIGHT - TANK-HEIGHT)
(check-expect (fire! (make-tank 28 3)) (make-posn 28 (- WORLD-HEIGHT TANK-HEIGHT)))
(check-expect (fire! (make-tank 103 -5)) (make-posn 103 (- WORLD-HEIGHT TANK-HEIGHT)))
(define (fire! t)
  (make-posn (tank-loc t) (- WORLD-HEIGHT TANK-HEIGHT)))

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

; sigs?: Any -> Boolean
; Determines whether `v` is a sigs according to our data definition
;
; (In a revised version, SIGS is a structure, but `define-struct` defines a predicate;
; which is why I chose this interpretation for this exercise)
(define (sigs? v)
  (or (aim? v) (fired? v)))
