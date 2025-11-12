#lang htdp/bsl

;; The Manhattan distance of a point to the origin considers a path that follows the
;; rectangular grid of streets found in Manhattan. Does it matter which strategy you
;; follow? (No, as long as we keep moving towards the origin in either axis, it does not
;; matter)
;;
;; Design the function `manhattan-distance`, which measures the Manhattan distance of
;; the given `posn` to the origin

; manhattan-distance: Posn -> Number
; Calculates the Manhattan distance of `p`, to the origin (0, 0)
(check-expect (manhattan-distance (make-posn 5 0)) 5)
(check-expect (manhattan-distance (make-posn 0 7)) 7)
(check-expect (manhattan-distance (make-posn 3 4)) 7)
(check-expect (manhattan-distance (make-posn 15 7)) 22)
(define (manhattan-distance p)
  (+ (posn-x p) (posn-y p)))
