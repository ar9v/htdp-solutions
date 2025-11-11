#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; Finish the design of a world program that simulates the traffic light FSA. Here is the
;; main function:

; traffic-light-simulation: TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [to-draw tl-render]
            [on-tick tl-next 1]))

;; The function's argument is the initial state for the `big-bang` expression, which tells
;; DrRacket to redraw the state of the world with `tl-render` and to handle clock ticks
;; with `tl-next`. Also note that informs the computer that the clock should tick once per
;; second.

;; Complete the design of `tl-render` and `tl-next`. Start with copying `TrafficLight`,
;; `tl-next`, and `tl-render` into DrRacket's definitions area.

;;; A `TrafficLight` is one of the following `String`s
;;; -- "red"
;;; -- "green"
;;; -- "yellow"
;;; interpretation: the three strings represent the three
;;; possible states that a traffic light may assume.


;; Physical Constants
(define BULB-RADIUS 20)
(define WIDTH (* BULB-RADIUS 8))
(define HEIGHT (* BULB-RADIUS 4))

(define GREEN-X (* WIDTH 2/10))
(define YELLOW-X (* WIDTH 1/2))
(define RED-X (* WIDTH 8/10))
(define Y (/ HEIGHT 2))

;; Graphical Constants
(define BACKGROUND (empty-scene WIDTH HEIGHT "black"))
(define GREEN "green")
(define YELLOW "darkyellow")
(define RED "red")

; tl-next: TrafficLight -> TrafficLight
; yields the next state, given current state `cs`
(check-expect (tl-next GREEN) YELLOW)
(check-expect (tl-next YELLOW) RED)
(check-expect (tl-next RED) GREEN)
(define (tl-next cs)
  (cond [(string=? cs GREEN) YELLOW]
        [(string=? cs YELLOW) RED]
        [(string=? cs RED) GREEN]))

; tl-render: TrafficLight -> Image
; renders the current state `cs` as an image
(check-expect (tl-render GREEN)
              (place-image (bulb GREEN #true) GREEN-X Y
                           (place-image (bulb YELLOW #false) YELLOW-X Y
                                        (place-image (bulb RED #false) RED-X Y
                                                     BACKGROUND))))

(check-expect (tl-render YELLOW)
              (place-image (bulb GREEN #false) GREEN-X Y
                           (place-image (bulb YELLOW #true) YELLOW-X Y
                                        (place-image (bulb RED #false) RED-X Y
                                                     BACKGROUND))))
(check-expect (tl-render RED)
              (place-image (bulb GREEN #false) GREEN-X Y
                           (place-image (bulb YELLOW #false) YELLOW-X Y
                                        (place-image (bulb RED #true) RED-X Y
                                                     BACKGROUND))))
(define (tl-render cs)
  (place-image (bulb GREEN (string=? cs GREEN)) GREEN-X Y
               (place-image (bulb YELLOW (string=? cs YELLOW)) YELLOW-X Y
                            (place-image (bulb RED (string=? cs RED)) RED-X Y
                                         BACKGROUND))))

; bulb: TrafficLight Boolean -> Image
; Renders a bulb, which is a circle of color `tl`. It is filled if it is `on?`.
(check-expect (bulb GREEN #true) (circle BULB-RADIUS "solid" GREEN))
(check-expect (bulb GREEN #false) (circle BULB-RADIUS "outline" GREEN))
(check-expect (bulb YELLOW #true) (circle BULB-RADIUS "solid" YELLOW))
(check-expect (bulb YELLOW #false) (circle BULB-RADIUS "outline" YELLOW))
(check-expect (bulb RED #true) (circle BULB-RADIUS "solid" RED))
(check-expect (bulb RED #false) (circle BULB-RADIUS "outline" RED))
(define (bulb tl on?)
  (circle BULB-RADIUS (if on? "solid" "outline") tl))
