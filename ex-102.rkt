#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Design all other functions that are needed to complete the game for this second
;;; data definition.

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

(define-struct sigs [ufo tank missile])
; A SIGS is a structure:
;  (make-sigs UFO Tank MissileOrNot)
;
; interpretation: represents the complete state of a space invader game

; A MissileOrNot is one of:
; -- #false
; -- Posn
;
; interpretation: #false means the missile is in the tank; Posn says the missile is at
; that location

(define aiming (make-sigs (make-posn 20 10) (make-tank 28 -3) #false))
(define about-to-hit
  (make-sigs (make-posn 20 100)
             (make-tank 100 3)
             (make-posn 22 103)))
(define just-fired
  (make-sigs (make-posn 20 100)
             (make-tank 100 3)
             (make-posn 22 (- WORLD-HEIGHT TANK-HEIGHT MISSILE-SIZE))))
(define landed-while-aiming
  (make-sigs (make-posn 50 (- WORLD-HEIGHT UFO-RADIUS))
             (make-tank 20 5)
             #false))
(define landed-after-firing
  (make-sigs (make-posn 50 (- WORLD-HEIGHT UFO-RADIUS))
             (make-tank 150 -3)
             (make-posn 150 10)))

; si-main: SIGS -> SIGS
; Runs the game
(define (si-main s)
  (big-bang s
            [to-draw si-render]
            [on-tick si-move CLOCK-RATE]
            [on-key si-control]
            [stop-when si-game-over? si-render-final]))

; si-render: SIGS -> Image
; adds TANK, UFO, and possibly MISSILE to the BACKGROUND scene
(define (si-render s)
  (tank-render (sigs-tank s)
               (ufo-render (sigs-ufo s)
                           (missile-render (sigs-missile s)
                                           BACKGROUND))))

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

; missile-render: MissileOrNot Image -> Image
; adds an image of missile m to scene s
(check-expect (missile-render #false (empty-scene 200 200))
              (empty-scene 200 200))
(check-expect (missile-render (make-posn 32 (- WORLD-HEIGHT TANK-HEIGHT 10))
                              (empty-scene WORLD-WIDTH WORLD-HEIGHT))
              (place-image
               MISSILE
               32 (- WORLD-HEIGHT TANK-HEIGHT 10)
               (empty-scene WORLD-WIDTH WORLD-HEIGHT)))
(define (missile-render m s)
  (cond [(false? m) s]
        [(posn? m) (place-image MISSILE (posn-x m) (posn-y m) s)]))

; si-move: SIGS -> SIGS
; Determines the next SIGS state
(define (si-move s)
  (make-sigs (move-ufo (sigs-ufo s))
             (move-tank (sigs-tank s))
             (move-missile (sigs-missile s))))

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
(check-expect (move-tank (sigs-tank aiming))
              (make-tank (+ (tank-loc (sigs-tank aiming))
                            (tank-vel (sigs-tank aiming)))
                         (tank-vel (sigs-tank aiming))))
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

; move-missile: MissileOrFalse -> MissileOrFalse
; Moves the missile upwards in a straight line if it's a Posn. Returns it otherwise
(check-expect (move-missile #false) #false)
(check-expect (move-missile (make-posn 50 150))
              (make-posn 50 (- 150 MISSILE-SPEED)))
(define (move-missile m)
  (if (false? m) #false (make-posn (posn-x m) (- (posn-y m) MISSILE-SPEED))))

; si-control: SIGS KeyEvent -> SIGS
; Produces a new game event according to the following:
; - pressing the left arrow ensures the tank moves left;
; - pressing the right arrow ensures the tank moves right; and
; - pressing the space bar fires the missile if it hasn't been launched yet.
(check-expect (si-control aiming "left") (sigs-tank-left aiming))
(check-expect (si-control aiming "right") (sigs-tank-right aiming))
(check-expect (si-control aiming " ")
              (make-sigs (sigs-ufo aiming) (sigs-tank aiming) (fire! (sigs-tank aiming))))
(check-expect (si-control aiming "a") aiming)

(check-expect (si-control just-fired "left") (sigs-tank-left just-fired))
(check-expect (si-control just-fired "right") (sigs-tank-right just-fired))
(check-expect (si-control just-fired " ") just-fired)
(check-expect (si-control just-fired "a") just-fired)
(define (si-control s ke)
  (cond [(key=? ke "left") (sigs-tank-left s)]
        [(key=? ke "right") (sigs-tank-right s)]
        [(and (key=? ke " ") (false? (sigs-missile s)))
         (make-sigs (sigs-ufo s) (sigs-tank s) (fire! (sigs-tank s)))]
        [else s]))

; sigs-tank-left: SIGS -> SIGS
; Shifts SIGS `s`'s tank velocity sign to a negative sign
(check-expect (sigs-tank-left aiming) aiming)
(check-expect (sigs-tank-left (make-sigs (make-posn 50 50) (make-tank 100 3) #false))
              (make-sigs (make-posn 50 50) (make-tank 100 -3) #false))
(check-expect (sigs-tank-left just-fired)
              (make-sigs (sigs-ufo just-fired)
                         (make-tank 100 -3)
                         (sigs-missile just-fired)))
(check-expect (sigs-tank-left (make-sigs (make-posn 50 50)
                                         (make-tank 100 -3)
                                         (make-posn 100 100)))
              (make-sigs (make-posn 50 50)
                         (make-tank 100 -3)
                         (make-posn 100 100)))
(define (sigs-tank-left s)
  (make-sigs (sigs-ufo s)
             (tank-vel-left (sigs-tank s))
             (sigs-missile s)))

; sigs-tank-right: SIGS -> SIGS
; Shifts SIGS `s`'s tank velocity sign to a positive sign
(check-expect (sigs-tank-right aiming)
              (make-sigs (make-posn 20 10) (make-tank 28 3) #false))
(check-expect (sigs-tank-right (make-sigs (make-posn 50 50) (make-tank 100 3) #false))
              (make-sigs (make-posn 50 50) (make-tank 100 3) #false))
(check-expect (sigs-tank-right just-fired) just-fired)
(check-expect (sigs-tank-right (make-sigs (make-posn 50 50)
                                          (make-tank 100 -3)
                                          (make-posn 100 100)))
              (make-sigs (make-posn 50 50)
                         (make-tank 100 3)
                         (make-posn 100 100)))
(define (sigs-tank-right s)
  (make-sigs (sigs-ufo s)
             (tank-vel-right (sigs-tank s))
             (sigs-missile s)))

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
  (or (hit-by-missile? (sigs-ufo s) (sigs-missile s))
      (landed? (sigs-ufo s))))

; hit-by-missile?: UFO MissileOrFalse -> Boolean
; returns true if `ufo` and `missile` have `collided?`. If MissileOrFalse is false, return
; false.
(check-expect (hit-by-missile? (sigs-ufo just-fired) (sigs-missile just-fired))
              #false)
(check-expect (hit-by-missile? (sigs-ufo about-to-hit) (sigs-missile about-to-hit))
              #true)
(check-expect (hit-by-missile? (sigs-ufo aiming) (sigs-missile aiming)) #false)
(define (hit-by-missile? ufo m)
  (cond [(false? m) #false]
        [(posn? m) (<= (distance ufo m) UFO-RADIUS)]))

; landed?: UFO -> Boolean
; returns true if `ufo` has reached the bottom of the BACKGROUND
(check-expect (landed? (sigs-ufo aiming)) #false)
(check-expect (landed? (sigs-ufo landed-while-aiming)) #true)
(check-expect (landed? (sigs-ufo about-to-hit)) #false)
(check-expect (landed? (sigs-ufo just-fired)) #false)
(check-expect (landed? (sigs-ufo landed-after-firing)) #true)
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
   (cond [(landed? (sigs-ufo s))
          (text "Game Over" FONT-SIZE GAME-OVER-FONT-COLOR)]
         [else
          (text "You Win!" FONT-SIZE GAME-WON-FONT-COLOR)])
   (si-render s)))
