#lang htdp/bsl

;;; Design the function `inner`, which consumes an RD an produces the (color of the)
;;; innermost doll. Use DrRacket's stepper to evaluate `(inner rd)` for your favorite `rd`.

(define-struct layer [color doll])

; inner: RD -> String
; Produces the color of the innermost doll in `rd`.
(check-expect (inner "red") "red")
(check-expect (inner (make-layer "yellow" (make-layer "red" "green"))) "green")
(define (inner rd)
  (cond [(string? rd) rd]
        [(layer? rd) (inner (layer-doll rd))]))
