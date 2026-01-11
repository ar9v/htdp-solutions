#lang htdp/isl

;;; Evaluate `(squared>? 3 10)` `(squared>? 4 10)` in DrRacket. How about
;;; `(squared>? 5 10)`

; squared>?: Number Number -> Boolean
; is the area of a square with side `x` larger than `c`
(define (squared>? x c)
  (> (sqr x) c))

(squared>? 3 10) ; #false
(squared>? 4 10) ; #true
(squared>? 5 10) ; #true
