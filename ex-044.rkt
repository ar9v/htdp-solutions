#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; Formulate the examples as BSL tests. Click RUN and watch them fail.

; hyper: WorldState Number Number String -> WorldState
; places the car at `x-mouse`
; if the given `me` is "button down"
;
; given: 21 10 20 "enter", expect: 21
; given: 42 10 20 "button-down", expect: 10
; given: 42 10 20 "move", expect: 42
(define (hyper x-position-of-car x-mouse y-mouse me)
  x-position-of-car)

(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") 10)
(check-expect (hyper 42 10 20 "move") 42)
