#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; Design a `big-bang` program that simulates a traffic light for a given duration.
;; The program renders the state of a traffic light as a solid circle of the appropriate
;; color, and it changes state on every clock tick. What is the most appropriate initial
;; state? Ask your engineering friends.

;;; A `TrafficLight` is one of the following `String`s
;;; -- "red"
;;; -- "green"
;;; -- "yellow"
;;; interpretation: the three strings represent the three
;;; possible states that a traffic light may assume.

;;; Physical Constants
(define RADIUS 50)
(define SCENE-WIDTH (* RADIUS 3))
(define SCENE-HEIGHT SCENE-WIDTH)

(define DURATION 1) ; seconds

;;; Graphical Constants
(define BACKGROUND (empty-scene SCENE-WIDTH SCENE-HEIGHT))


;;; traffic-light-prog: TrafficLight -> TrafficLight
;;; Runs the traffic light simulation
;;;
;;; Up to this point, the book says nothing about `on-tick` being able to take another
;;; expression, `rate-expr`. They do call for reading the docs though... so while I want
;;; to write these with what we've been given so far, I think this is fair game.
(define (traffic-light-prog traffic-light)
  (big-bang traffic-light
            [to-draw render]
            [on-tick traffic-light-next DURATION]))

;;; render: TrafficLight -> Image
;;; This renders a circle of the color given by `traffic-light` in the BACKGROUND.
;;; If the expression given as `traffic-light` is not a TrafficLight, returns BACKGROUND.
(check-expect (render "green") (overlay (circle RADIUS "solid" "green") BACKGROUND))
(check-expect (render "yellow") (overlay (circle RADIUS "solid" "yellow") BACKGROUND))
(check-expect (render "red") (overlay (circle RADIUS "solid" "red") BACKGROUND))
(check-expect (render "blue") BACKGROUND)
(define (render traffic-light)
  (if (traffic-light? traffic-light)
      (overlay (circle RADIUS "solid" traffic-light) BACKGROUND)
      BACKGROUND))

;;; traffic-light?: Any -> Boolean
;;; Given an expression `e`, determine if it is a member of TrafficLight
;;;
;;; NOTE: The book doesn't touch upon checked functions at this point, so this might be
;;; cheating a bit. This is still less than ideal, and doesn't use functions or concepts
;;; that are too far-fetched at this point in the book.
;;;
;;; Alternatively, one could document the assumption that `render` (and other functions
;;; that take a TrafficLight) will always receive a valid TrafficLight.
(check-expect (traffic-light? "green") #true)
(check-expect (traffic-light? "yellow") #true)
(check-expect (traffic-light? "red") #true)
(check-expect (traffic-light? "blue") #false)
(check-expect (traffic-light? 1) #false)
(check-expect (traffic-light? #true) #false)
(define (traffic-light? e)
  (and (string? e) (or (string=? e "green")
                       (string=? e "yellow")
                       (string=? e "red"))))

; traffic-light-next: TrafficLight -> TrafficLight
; yields the next state given current state `s`
;
; NOTE: no need to use `traffic-light?` here; `cond` will error if another thing is passed
; in.
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next "yellow") "red")
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))
