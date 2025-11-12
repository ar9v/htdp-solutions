#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; During a door simulation the "open" state is barely visible. Modify `door-simulation`
;; so that the clock ticks once every three seconds. Rerun the simulation

(define TICK-RATE 3)

(define LOCKED "locked")
(define CLOSED "closed")
(define OPEN "open")

; A DoorState is one of:
; -- LOCKED
; -- CLOSED
; -- OPEN

; door-closer: DoorState -> DoorState
; closes an open door over the period of one tick
(check-expect (door-closer LOCKED) LOCKED)
(check-expect (door-closer CLOSED) CLOSED)
(check-expect (door-closer OPEN) CLOSED)
(define (door-closer state-of-door)
  (cond [(string=? LOCKED state-of-door) LOCKED]
        [(string=? CLOSED state-of-door) CLOSED]
        [(string=? OPEN state-of-door) CLOSED]))

; door-action: DoorState KeyEvent -> DoorState
; turns key event `k` into an action or state `s`
(check-expect (door-action LOCKED "u") CLOSED)
(check-expect (door-action CLOSED "l") LOCKED)
(check-expect (door-action CLOSED " ") OPEN)
(check-expect (door-action OPEN "a") OPEN)
(check-expect (door-action CLOSED "a") CLOSED)
(define (door-action s k)
  (cond [(and (string=? LOCKED s) (string=? "u" k)) CLOSED]
        [(and (string=? CLOSED s) (string=? "l" k)) LOCKED]
        [(and (string=? CLOSED s) (string=? " " k)) OPEN]
        [else s]))

; door-render: DoorState -> Image
; translates the state `s` into a large text image
(define (door-render s)
  (text s 40 "red"))

; door-simulation: DoorState -> DoorState
; simulates a door with an automatic door closer
(define (door-simulation initial-state)
  (big-bang initial-state
            [to-draw door-render]
            [on-tick door-closer TICK-RATE]
            [on-key door-action]))
