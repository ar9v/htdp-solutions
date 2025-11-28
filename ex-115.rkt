#lang htdp/bsl

;;; Revise `light=?` so that the error message specifies which of the two arguments
;;; isn't an element of TrafficLight.

; light?: Any -> Boolean
; is the given value an element of TrafficLight
(define (light? x)
  (cond
    [(string? x) (or (string=? "red" x)
                     (string=? "green" x)
                     (string=? "yellow" x))]
    [else #false]))

; light=?: Any Any -> Boolean
; are the two values elements of TrafficLight and, if so, are they equal
(check-expect (light=? "red" "red") #true)
(check-expect (light=? "red" "green") #false)
(check-expect (light=? "green" "green") #true)
(check-expect (light=? "yellow" "yellow") #true)
(define (light=? a-value another-value)
  (cond [(and (not (light? a-value)) (not (light? another-value)))
         (error "light=? expects traffic lights as args, given: "
                a-value
                " and "
                another-value)]
        [(not (light? a-value))
         (error "traffic light expected, but first value was: " a-value)]
        [(not (light? another-value))
         (error "traffic light expected, but second value was: " another-value)]
        [else
         (string=? a-value another-value)]))
