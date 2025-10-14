#lang htdp/bsl

(require 2htdp/image)

;; Add the following line to the definitions area of DrRacket:
(define in "hello world")

;; Then create an expression that converts the value of `in` to a positive number.
;; For a String, it determines how long the string is.
;; For an Image, it uses the area.
;; For a number, it decrements the number by 1, unless it is already 0 or negative
;; For #true, it uses 10
;; For #false, it uses 20
;;
;; Technically we haven't "seen" cond, but it _is_ in the Prologue, so...
(cond [(string? in) (string-length in)]
      [(image? in)  (* (image-width in) (image-height in))]
      [(number? in) (if (positive? in) (sub1 in) in)]
      [(false? in) 20]
      [in 10])

;; This evaluates to 11
