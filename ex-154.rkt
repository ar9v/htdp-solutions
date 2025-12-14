#lang htdp/bsl

;;; Design the function `colors`. It consumes a Russian doll and produces a string of
;;; all colors, separated by a comma and a space. Thus our example should produce
;;;
;;; "yellow, green, red"

(define-struct layer [color doll])

; colors: RD -> String
; Produces a formatted string of all the colors contained within `rd`.
(check-expect (colors "red") "red")
(check-expect (colors (make-layer "yellow" (make-layer "green" "red")))
              "yellow, green, red")
(define (colors rd)
  (cond [(string? rd) rd]
        [(layer? rd)
         (string-append (layer-color rd) ", " (colors (layer-doll rd)))]))
