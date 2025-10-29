#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; Let's work through the same problem statement with a time-based data definition:

; An AnimationState is a Number
; interpretation: the number of clock ticks since the animation started

;;; Like the original data definition, this one also equates the states of the world with
;;; the class of numbers. Its interpretation, however, explains that the number means
;;; something entirely different.

;;; Design the functions `tock` and `render`. Then develop a `big-bang` expression so that
;;; once again you get an animation of a car traveling from left to right across the
;;; world's canvas.

;;; Physical Constants
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 3))
(define BODY-WIDTH (* WHEEL-RADIUS 8))
(define BODY-HEIGHT (* WHEEL-RADIUS 2))
(define ROOF-WIDTH (* WHEEL-RADIUS 4))
(define ROOF-HEIGHT (* WHEEL-RADIUS 1.5))
(define WORLD-WIDTH 500)
(define WORLD-HEIGHT (* WHEEL-RADIUS 10))
(define Y-CAR (- WORLD-HEIGHT (/ (+ WHEEL-RADIUS BODY-HEIGHT ROOF-HEIGHT) 2)))
(define X-LIMIT (+ WORLD-WIDTH BODY-WIDTH))
(define VELOCITY 3)

;;; Graphical Constants
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


;;; main: AnimationState -> AnimationState
;;; Launches the program to animate the car
(define (main as)
  (big-bang as
            [on-tick tock]
            [to-draw render]
            [stop-when out-of-bounds?]))

;;; render: AnimationState -> Image
;;; Places CAR in the BACKGROUND, at a distance of VELOCITY * as pixels from the left.
(check-expect (render 0) (place-image CAR 0 Y-CAR BACKGROUND))
(check-expect (render 1) (place-image CAR (* VELOCITY 1) Y-CAR BACKGROUND))
(check-expect (render 2) (place-image CAR (* VELOCITY 2) Y-CAR BACKGROUND))
(define (render as)
  (place-image CAR (* VELOCITY as) Y-CAR BACKGROUND))

;;; tock: AnimationState -> AnimationState
;;; Adds 1 to the current `as`
(check-expect (tock 0) 1)
(check-expect (tock 1) 2)
(define (tock as) (add1 as))

;;; out-of-bounds?: AnimationState -> Boolean
;;; Returns true when the car has reached at least X-LIMIT
(check-expect (out-of-bounds? 0) #false)
(check-expect (out-of-bounds? (/ (time X-LIMIT) 2)) #false)
(check-expect (out-of-bounds? (time X-LIMIT)) #true)
(check-expect (out-of-bounds? (time (add1 X-LIMIT))) #true)
(define (out-of-bounds? as) (>= (distance as) X-LIMIT))

;;; time: Number -> AnimationState
;;; Given a distance, `d`, returns the time it would take to cover `d` given VELOCITY
;;;
;;; assumptions: VELOCITY is non-zero.
(check-expect (time 0) 0)
(check-expect (time 5) (/ 5 VELOCITY))
(define (time d) (/ d VELOCITY))

;;; distance: AnimationState -> Number
;;; Given an AnimationState `as` (a time), compute the distance traveled with VELOCITY
(check-expect (distance 0) 0)
(check-expect (distance 5) (* VELOCITY 5))
(define (distance time) (* VELOCITY time))

;;; How do you think this program relates to `animate` from the Prologue?
;;;
;;; A:
;;; They're essentially the same! `animate`'s argument is essentially `render`. So
;;; `animate` is really a `big-bang` special case (since it doesn't have handlers for
;;; other types of events).


;;; Use the new data definition to design a program that moves the car according to a
;;; sine wave.

;;; NOTE: Ideally, one would share as much logic as possible (e.g. having a single
;;;       `distance` function); I'm writing separate versions of most
;;;       functions in the interest making it easy to run the whole file and check both
;;;       things, without going back and forth between swapping definitions.

;;; (Extra) Physical Constants
;;; NOTE: You do you. This SINE-OFFSET + PHASE combo makes it so the car starts
;;;       "on the ground" and keeps it on it.
(define AMPLITUDE 10)
(define SINE-OFFSET (- WORLD-HEIGHT (/ (image-height CAR) 2) AMPLITUDE))
(define PHASE (/ pi 2))
(define ANGULAR-FREQUENCY 1/4)

;;; sine-main: AnimationState -> AnimationState
;;; Launches the program that animates a car w/ the motion of a sine wave
(define (sine-main as)
  (big-bang as
            [on-tick tock]
            [to-draw sine-render]
            [stop-when sine-out-of-bounds?]))

;;; sine-render: AnimationState -> Image
;;; Places the CAR in the BACKGROUND using a sine function to determine x and y
(check-expect (sine-render 0) (place-image CAR (x-car 0) (y-car 0) BACKGROUND))
(check-expect (sine-render 1) (place-image CAR (x-car 1) (y-car 1) BACKGROUND))
(check-expect (sine-render 10) (place-image CAR (x-car 10) (y-car 10) BACKGROUND))
(define (sine-render as)
  (place-image CAR (x-car as) (y-car as) BACKGROUND))

;;; y-car: AnimationState -> Number
;;; Computes the y-coordinate of CAR given a number, `as`, which is an AnimationState
;;; No examples, since `check-expect` cannot compare inexact numbers
;;; (spoiler: `check-within` is introduced later)
(define (y-car t)
  (+ (* AMPLITUDE (sin (+ (* ANGULAR-FREQUENCY t)
                          PHASE)))
     SINE-OFFSET))

;;; x-car: AnimationState -> Number
;;; Alias for `distance`, for this part of the exercise
;;; Given an AnimationState `as` (a time), compute the distance traveled with VELOCITY
(check-expect (distance 0) 0)
(check-expect (distance 5) (* VELOCITY 5))
(define (x-car t) (+ (* VELOCITY t) (/ BODY-WIDTH 2)))

;;; sine-out-of-bounds?: AnimationState -> Boolean
;;; True if the car is at an x-coordinate that's at least X-LIMIT
(check-expect (sine-out-of-bounds? 0) #false)
(check-expect (sine-out-of-bounds? 4) #false)
(check-expect (sine-out-of-bounds? (x-car (time X-LIMIT))) #true)
(define (sine-out-of-bounds? t)
  (>= (x-car t) X-LIMIT))
