#lang htdp/bsl+

(require 2htdp/image)

;;; Design `state=?`, an equality predicate for states

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
