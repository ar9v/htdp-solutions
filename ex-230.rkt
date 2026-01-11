#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Consider the following data representation for FSMs:

(define-struct fsm [initial transitions final])
(define-struct transition [current key next])
; An FSM is a structure
;   (make-fsm FSM-State LOT FSM-State)
;
; A LOT is one of
; -- '()
; -- (cons Transition LOT)
;
; A Transition is a structure:
;   (make-transition FSM-State KeyEvent FSM-State)

; A RegexpState is one of
(define START "white")
(define A "yellow")
(define BC "yellow")
(define D "green")

;;; Represent the FSM from exercise 109 in this context.
(define a-bc-d
  (make-fsm START
            (list (make-transition START "a" A)
                  (make-transition A "b" BC)
                  (make-transition A "c" BC)
                  (make-transition BC "b" BC)
                  (make-transition BC "c" BC)
                  (make-transition BC "d" D))
            D))

;;; Design the function `fsm-simulate`, which accepts an FSM and runs it on a player's
;;; keystrokes. If the sequence of keystrokes forces the FSM to reach a final state,
;;; `fsm-simulate` stops.

; fsm-simulate: FSM -> FSM
; Runs the finite state machine simulation
(define (fsm-simulate a-fsm)
  (big-bang a-fsm
            [to-draw state-as-colored-square]
            [on-key find-next-state]
            [stop-when end-state? state-as-colored-square]))

; state-as-colored-square: FSM -> Image
; Renders `a-fsm`'s current state as a colored square
(check-expect (state-as-colored-square a-bc-d)
              (square 100 "solid" (fsm-initial a-bc-d)))
(define (state-as-colored-square a-fsm)
  (square 100 "solid" (fsm-initial a-fsm)))

; find-next-state: FSM KeyEvent -> FSM
; Picks out the next FSM state based on `ke`
(check-expect (find-next-state a-bc-d "a")
              (make-fsm A (fsm-transitions a-bc-d) (fsm-final a-bc-d)))
(check-error (find-next-state a-bc-d "k")
             "no transition found for state 'white' and key 'k'")
(define (find-next-state a-fsm ke)
  (make-fsm (find-state (fsm-initial a-fsm) ke (fsm-transitions a-fsm))
            (fsm-transitions a-fsm)
            (fsm-final a-fsm)))

; find-state: FSM-State KeyEvent LOT -> FSM-State
; Finds the state we'd arrive to from `s0` with `ke`
(check-expect (find-state (fsm-initial a-bc-d)
                          "a"
                          (fsm-transitions a-bc-d))
              A)
(check-expect (find-state BC
                          "d"
                          (fsm-transitions a-bc-d))
              D)
(check-error (find-state BC
                         "a"
                         (fsm-transitions a-bc-d))
             "no transition found for state 'yellow' and key 'a'")
(define (find-state s0 ke lot)
  (cond [(empty? lot)
         (error "no transition found for state '" s0 "' and key '" ke "'")]
        [(cons? lot)
         (if (and (state=? s0 (transition-current (first lot)))
                  (key=? ke (transition-key (first lot))))
             (transition-next (first lot))
             (find-state s0 ke (rest lot)))]))

; end-state? FSM -> Boolean
; True when `a-fsm` has reached its `fsm-final` state
(check-expect (end-state? a-bc-d) #false)
(check-expect (end-state? (make-fsm D (fsm-transitions a-bc-d) (fsm-final a-bc-d)))
              #true)
(define (end-state? a-fsm)
  (state=? (fsm-initial a-fsm) (fsm-final a-fsm)))

; state=?: FSM-State FSM-State -> Boolean
; Returns true if s1 and s2 are `equal?`
(define (state=? fsm-state1 fsm-state2)
  (cond [(not (image-color? fsm-state1))
         (error "Error: a non-color value was provided for fsm-state1. Provided: "
                fsm-state1)]
        [(not (image-color? fsm-state2))
         (error "Error: a non-color value was provided for fsm-state2. Provided: "
                fsm-state2)]
        [else (equal? fsm-state1 fsm-state2)]))
