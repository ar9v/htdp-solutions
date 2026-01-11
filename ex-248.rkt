#lang htdp/isl

;;; Evaluate `(squared>? 3 10)` and `(squared> 4 10)` in DrRacket's stepper.

; squared>?: Number Number -> Boolean
; is the area of a square with side `x` larger than `c`
(define (squared>? x c)
  (> (sqr x) c))

(squared>? 3 10)
;; (squared>? 3 10)
;; ==
;; (> (sqr 3) 10)
;; ==
;; (> 9 10)
;; ==
;; #false

(squared>? 4 10)
;; (squared>? 4 10)
;; ==
;; (> (sqr 4) 10)
;; ==
;; (> 16 10)
;; ==
;; #true
