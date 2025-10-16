#lang htdp/bsl

;; Use DrRacket's stepper on this program fragment:
(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))

(distance-to-origin 3 4)

;; Does the explanation match your intuition?
;;
;; (distance-to-origin 3 4)
;; ===
;; (sqrt (+ (sqr 3) (sqr 4)))
;; ===
;; (sqrt (+ 9 (sqr 4)))
;; ===
;; (sqrt (+ 9 16))
;; ===
;; (sqrt 25)
;; ===
;; 5
;;
;; It does match, but I assumed arguments would be evaluated from left to right...
;; So it's not as obvious as it may seem (-:
