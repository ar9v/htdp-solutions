#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; In its default state, a pedestrian crossing light shows an orange person standing on
;;; a red background. When it is time to allow the pedestrian to cross the street, the
;;; light receives a signal and switches to a green, walking person. This phase lasts for
;;; 10 seconds. After that the light displays the digits 9, 8,...,0 with odd numbers
;;; colored orange and even numbers colored green. When the countdown reaches 0, the light
;;; switches back to its default state.

;;; Design a world program that implements such a pedestrian traffic light. The light
;;; switches from its default state when you press the space bar on your keyboard. All
;;; other transitions must be reactions to clock ticks.

(define GREEN-LIGHT (bitmap/file "images/pedestrian_traffic_light_green.png"))
(define RED-LIGHT (bitmap/file "images/pedestrian_traffic_light_red.png"))

; Physical Constants
(define LIGHT-WIDTH (image-width GREEN-LIGHT))
(define LIGHT-HEIGHT (image-height GREEN-LIGHT))

(define WORLD-WIDTH (* 2 LIGHT-WIDTH))
(define WORLD-HEIGHT (* 2 LIGHT-HEIGHT))

(define FONT-SIZE LIGHT-HEIGHT)

; Graphical Constants
(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT "black"))

; A TrafficLightState is one of
; -- STOP
; -- Go
; -- Countdown
(define STOP "stop")

(define-struct go [secs])
; A Go is a structure
;   (make-go Number)
;
; interpretation:
; (make-go n) represents the state when the traffic light is green, and there are
; n seconds remaining. In our program, n is in [0, 10], meaning, we'll see the green
; light for 10 seconds; (make-go 0) is a signal to shift to Countdown.

; a Countdown is a Number in [0, 9]
; interpretation: it represents the amount of seconds before going back to STOP.

; pedestrian-light: TrafficLightState -> TrafficLightState
; Kicks off the pedestrian light simulation
(define (pedestrian-light tl-state)
  (big-bang tl-state
            [to-draw render]
            [on-tick update-tl 1]
            [on-key handle-key]))

; render: TrafficLightState -> Image
; Draws the current traffic light state
(check-expect (render STOP) (overlay RED-LIGHT BACKGROUND))
(check-expect (render (make-go 10)) (overlay GREEN-LIGHT BACKGROUND))
(check-expect (render 9) (overlay (render-countdown 9) BACKGROUND))
(define (render tl-state)
  (cond [(equal? tl-state STOP) (overlay RED-LIGHT BACKGROUND)]
        [(go? tl-state) (overlay GREEN-LIGHT BACKGROUND)]
        [(number? tl-state)
         (overlay (render-countdown tl-state) BACKGROUND)]))

; render-countdown: Countdown -> Image
; Renders Countdown `cd` as a `text`, coloring it orange if it is odd and green otherwise
(check-expect (render-countdown 1) (text "1" FONT-SIZE "orange"))
(check-expect (render-countdown 2) (text "2" FONT-SIZE "green"))
(define (render-countdown cd)
  (text (number->string cd) FONT-SIZE (if (odd? cd) "orange" "green")))

; update-tl: TrafficLightState -> TrafficLightState
; Produces a new tl-state depending on the current one
(check-expect (update-tl STOP) STOP)
(check-expect (update-tl (make-go 10)) (go-secs-dec (make-go 10)))
(check-expect (update-tl (make-go 0)) 9)
(check-expect (update-tl 10) 9)
(check-expect (update-tl 0) STOP)
(define (update-tl tl-state)
  (cond [(equal? tl-state STOP) STOP]
        [(go? tl-state) (if (= (go-secs tl-state) 0) 9 (go-secs-dec tl-state))]
        [(number? tl-state)
         (if (= tl-state 0) STOP (sub1 tl-state))]))

; go-secs-dec: Go -> Go
; Return a new Go with `secs` - 1
(define (go-secs-dec g) (make-go (sub1 (go-secs g))))

; handle-key: TrafficLightState KeyEvent -> TrafficLightState
; If the user presses SPACE and the current state is STOP, trigger a change to GoState.
; Do nothing otherwise
(check-expect (handle-key STOP " ") (make-go 10))
(check-expect (handle-key STOP "a") STOP)
(check-expect (handle-key (make-go 10) " ") (make-go 10))
(check-expect (handle-key (make-go 10) "a") (make-go 10))
(check-expect (handle-key 10 " ") 10)
(check-expect (handle-key 10 "a") 10)
(define (handle-key tl-state ke)
  (if (and (equal? tl-state STOP) (key=? ke " "))
      (make-go 10)
      tl-state))

(pedestrian-light STOP)
