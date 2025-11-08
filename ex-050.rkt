#lang htdp/bsl

;; If you copy and paste the above function (`traffic-light-next` in Chapter 4.3) into
;; the definitions area of DrRacket and click RUN, DrRacket highlights two of the three
;; `cond` lines. This coloring tells you that your test cases do not cover the full
;; conditional. Add enough tests to make DrRacket happy.


; traffic-light-next: TrafficLight -> TrafficLight
; yields the next state given current state `s`
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next "yellow") "red")
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))
