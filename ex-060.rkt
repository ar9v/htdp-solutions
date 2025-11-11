#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; An alternative data representation for a traffic light program may use numbers instead
;; of strings:

; An N-TrafficLight is one of:
; -- 0 interpretation: the traffic light shows red
; -- 1 interpretation: the traffic light shows green
; -- 2 interpretation: the traffic light shows yellow

;; It greatly simplifies the definition of `tl-next`

; tl-next-numeric: N-TrafficLight -> N-TrafficLight
; yields the next state, given the current state `cs`
(check-expect (tl-next-numeric 0) 1)
(check-expect (tl-next-numeric 1) 2)
(check-expect (tl-next-numeric 2) 0)
(define (tl-next-numeric cs) (modulo (+ cs 1) 3))

;; Does the `tl-next` function convey its intention more clearly than the `tl-next-numeric`
;; function? If so, why? If not, why not?

;;; A:
;;;
;;; I think it does! Using numbers forces us to refer back to the data definition to decode
;;; the meaning of the numbers. In the original version, we can clearly see how one state
;;; changes to the next _and_ how that ties to the original problem statement.
