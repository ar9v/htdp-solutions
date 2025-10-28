#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Modify the interpretaion of the sample data definition so that a state denotes the
;;; x-coordinate of the rightmost edge of the car.
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 3))
(define BODY-WIDTH (* WHEEL-RADIUS 8))
(define BODY-HEIGHT (* WHEEL-RADIUS 2))
(define ROOF-WIDTH (* WHEEL-RADIUS 4))
(define ROOF-HEIGHT (* WHEEL-RADIUS 1.5))
(define WORLD-WIDTH 500)
(define WORLD-HEIGHT (* WHEEL-RADIUS 10))
(define Y-CAR (- WORLD-HEIGHT (/ (+ WHEEL-RADIUS BODY-HEIGHT ROOF-HEIGHT) 2)))

;;; For purposes of this exercise, this definition also works! The only difference is that
;;; the car doesn't have extra leeway when it's out of sight.
(define X-LIMIT (+ WORLD-WIDTH BODY-WIDTH))


(define TREE
  (underlay/xy (circle (* WHEEL-RADIUS 2) "solid" "green")
               (* WHEEL-RADIUS 1.5) (* WHEEL-RADIUS 4)
               (rectangle WHEEL-RADIUS (* WHEEL-RADIUS 3) "solid" "brown")))

(define MTSCN (empty-scene WORLD-WIDTH WORLD-HEIGHT))
(define BACKGROUND-WITH-TREE
  (place-image
   TREE (* 2/3 WORLD-WIDTH)
   (- WORLD-HEIGHT (/ (image-height TREE) 2))
   MTSCN))
(define BACKGROUND BACKGROUND-WITH-TREE)

(define CAR-COLOR "red")
(define BODY (rectangle BODY-WIDTH BODY-HEIGHT "solid" CAR-COLOR))
(define ROOF (rectangle ROOF-WIDTH ROOF-HEIGHT "solid" CAR-COLOR))
(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define SPACE (rectangle WHEEL-DISTANCE WHEEL-RADIUS 0 "white"))
(define WHEELS (beside WHEEL SPACE WHEEL))

(define CAR
  (above ROOF
         (overlay/offset WHEELS 0 (- (/ BODY-HEIGHT 2)) BODY)))

;;; WorldState is a Number
;;; interpretaion: WorldState represents the x-coordinate of the rightmost edge of the car.

;;; main: WorldState -> WorldState
;;; Launches the program.
(define (main ws)
  (big-bang ws
            [on-tick tock]
            [to-draw render]
            [stop-when out-of-bounds?]))

;;; render: WorldState -> Image
;;; Places CAR in BACKGROUND, `ws` pixels from the left bound.
(check-expect (render 1) (place-image/align CAR 1 Y-CAR "right" "middle" BACKGROUND))
(check-expect (render 30) (place-image/align CAR 30 Y-CAR "right" "middle" BACKGROUND))
(check-expect (render -20) (place-image/align CAR -20 Y-CAR "right" "middle" BACKGROUND))
(define (render ws)
  (place-image/align CAR ws Y-CAR "right" "middle" BACKGROUND))

;;; tock: WorldState -> WorldState
;;; Moves the car 3 pixels per clock tick
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock ws) (+ ws 3))

;;; out-of-bounds?: WorldState -> Boolean
;;; True when the car has driven off the right side. The right side cut-off is given by
;;; the X-LIMIT constant.
(check-expect (out-of-bounds? 1) #false)
(check-expect (out-of-bounds? (/ WORLD-WIDTH 2)) #false)
(check-expect (out-of-bounds? (+ WORLD-WIDTH 1)) #false)
(check-expect (out-of-bounds? X-LIMIT) #true)
(define (out-of-bounds? ws)
  (>= ws X-LIMIT))
