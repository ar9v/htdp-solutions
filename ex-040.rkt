#lang htdp/bsl

(require 2htdp/image)

;; Formulate the examples as BSL tests, that is, using the `check-expect` form.
;; Introduce a mistake. Re-run the tests.

; tock: WorldState -> WorldState
; moves the car by 3 pixels for every clock tick
; examples:
;   given: 20, expect: 23
;   given: 78, expect: 81
(define (tock ws)
  (+ ws 3))

(check-expect (tock 20) 23)
(check-expect (tock 78) 81)

;;; Deliberately wrong example
(define (wrong-tock ws)
  (+ ws 4))

(check-expect (wrong-tock 20) 23)
(check-expect (wrong-tock 78) 81)
