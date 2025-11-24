#lang htdp/bsl

;;; Design `si-move`. This function is called for every clock tick to determine to which
;;; position the objects move now. Accordingly, it consumes an element of SIGS and
;;; produces another one.

; Physical Constants
(define TANK-WIDTH 40)
(define TANK-HEIGHT (/ TANK-WIDTH 3))

(define WORLD-WIDTH (* TANK-WIDTH 10))
(define WORLD-HEIGHT (* TANK-HEIGHT 30))

(define MISSILE-SIZE (/ TANK-WIDTH 2))
(define MISSILE-SPEED 1)

(define UFO-WING-WIDTH (* TANK-WIDTH 2))
(define UFO-RATE-OF-DESCENT 1)

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
; the y axis.
(check-random (move-ufo (make-posn 50 50))
              (make-posn (+ (random (- WORLD-WIDTH UFO-WING-WIDTH))
                            (/ UFO-WING-WIDTH 2))
                         (+ 50 UFO-RATE-OF-DESCENT)))
(define (move-ufo ufo)
  (make-posn (+ (random (- WORLD-WIDTH UFO-WING-WIDTH))
                (/ UFO-WING-WIDTH 2))
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
