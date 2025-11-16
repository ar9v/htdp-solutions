#lang htdp/bsl

;;; Design the function `time->seconds` which consumes instances of time structures
;;; (see Exercise 77) and produces the number of seconds that have passed since midnight.
;;;
;;; For example, if you are representing 12 hours, 30 minutes, and 2 seconds with one of
;;; these structures and if you then apply `time->seconds` to this instance, the correct
;;; result is 45002.

; a Time is a structure
;   (make-time Number Number Number)
;
; interpretation:
; (make-time h m s) represents a point in time `h` hours, `m` minutes and `s` seconds
; after midnight
;
; Assumptions:
;  hours is a Number in the range [0, 23]
;  minutes is a Number in the range [0, 59]
;  seconds is a Number in the range [0, 59]
(define-struct time [hours minutes seconds])

(define SECONDS-IN-MINUTE 60)
(define MINUTES-IN-HOUR 60)

; time->seconds: Time -> Number
; Calculates how many seconds t represents.
(check-expect (time->seconds (make-time 12 30 2)) 45002)
(check-expect (time->seconds (make-time 0 0 45)) 45)
(check-expect (time->seconds (make-time 0 2 45)) (+ (* 2 60) 45))
(check-expect (time->seconds (make-time 4 6 45))
              (+ (* 4 (sqr 60)) (* 6 60) 45))
(define (time->seconds t)
  (+ (* (time-hours t) MINUTES-IN-HOUR SECONDS-IN-MINUTE)
     (* (time-minutes t) SECONDS-IN-MINUTE)
     (time-seconds t)))
