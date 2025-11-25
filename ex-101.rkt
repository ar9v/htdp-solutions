#lang htdp/bsl

(require 2htdp/image)

;;; Turn the examples in figure 35 into test cases.

;;; Keeping it lean (i.e. sticking to the missile) just for convenience...
(define TANK-WIDTH 40)
(define TANK-HEIGHT (/ TANK-WIDTH 3))
(define WORLD-WIDTH (* TANK-WIDTH 10))
(define WORLD-HEIGHT (* TANK-HEIGHT 30))

(define MISSILE-SIZE (/ TANK-WIDTH 2))
(define MISSILE-COLOR "brown")

(define MISSILE
  (triangle MISSILE-SIZE "solid" MISSILE-COLOR))

; missile-render.v2: MissileOrNot Image -> Image
; adds an image of missile m to scene s
(check-expect (missile-render.v2 #false (empty-scene 200 200))
              (empty-scene 200 200))
(check-expect (missile-render.v2 (make-posn 32 (- WORLD-HEIGHT TANK-HEIGHT 10))
                                 (empty-scene WORLD-WIDTH WORLD-HEIGHT))
              (place-image
               MISSILE
               32 (- WORLD-HEIGHT TANK-HEIGHT 10)
               (empty-scene WORLD-WIDTH WORLD-HEIGHT)))
(define (missile-render.v2 m s)
  (cond [(false? m) s]
        [(posn? m) (place-image MISSILE (posn-x m) (posn-y m) s)]))
