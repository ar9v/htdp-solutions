#lang htdp/bsl

;;; Enter these definitions and their test cases into the definitions area of DrRacket and
;;; make sure they work. This is the first time that you have dealt with a "wish", and
;;; you need to make sure you understand how the two functions work together.

; a UFO is a structure:
;  (make-ufo [Posn Vel])
;
; interpretation:
; (make-ufo p v) is at location p moving at velocity v
(define-struct ufo [loc vel])

; a Vel is a structure:
;  (make-vel [deltax deltay])
;
; interpretation:
; (make-vel dx dy) represents a change in movement in the x axis as dx and in y as dy
(define-struct vel [deltax deltay])

(define v1 (make-vel 8 -3))
(define v2 (make-vel -5 -3))

(define p1 (make-posn 22 80))
(define p2 (make-posn 30 77))

(define u1 (make-ufo p1 v1))
(define u2 (make-ufo p1 v2))
(define u3 (make-ufo p2 v1))
(define u4 (make-ufo p2 v2))

; ufo-move-1: UFO -> UFO
; determines where ufo moves in one clock tick; leaves velocity as is
(check-expect (ufo-move-1 u1) u3)
(check-expect (ufo-move-1 u2) (make-ufo (make-posn 17 77) v2))
(define (ufo-move-1 ufo)
  (make-ufo (posn+ (ufo-loc ufo) (ufo-vel ufo))
            (ufo-vel ufo)))

; posn+: Posn Vel -> Posn
; adds Vel v to Posn p
(check-expect (posn+ p1 v1) p2)
(check-expect (posn+ p1 v2) (make-posn 17 77))
(define (posn+ p v)
  (make-posn (+ (posn-x p) (vel-deltax v))
             (+ (posn-y p) (vel-deltay v))))
