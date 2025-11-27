#lang htdp/bsl

;;; A checked version of `area-of-disk` can also enforce that the arguments to the
;;; function are positive numbers, not just arbitrary numbers. Modify
;;; `checked-area-of-disk` in this way.

(define MESSAGE "area-of-disk: positive number expected")

; area-of-disk: Number -> Number
; computes the area of a disk with radius r
(define (area-of-disk r)
  (* 3.14 (* r r)))

; checked-area-of-disk: Any -> NumberOrError
; validates that `v` is a positive number. If it is, calls `area-of-disk` with it; signals
; an error otherwise.
(define (checked-area-of-disk v)
  (cond [(and (number? v) (positive? v)) (area-of-disk v)]
        [else (error MESSAGE)]))
