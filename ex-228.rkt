#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Complete the design of `find`. Once the auxiliary functions are tested, use `simulate`
;;; to play with `fsm-traffic` and the BW Machine from exercise 227.

(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

(define-struct fs [fsm current])

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

(define bw-machine
  (list (make-transition "black" "white")
        (make-transition "white" "black")))

(define (simulate an-fsm s0)
  (big-bang (make-fs an-fsm s0)
            [to-draw state-as-colored-square]
            [on-key find-next-state]))

(define (state-as-colored-square current-fs)
  (square 100 "solid" (fs-current current-fs)))

(define (find-next-state current-fs ke)
  (make-fs
   (fs-fsm current-fs)
   (find (fs-fsm current-fs) (fs-current current-fs))))

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

; find: FSM FSM-State -> FSM-State
; finds the state representing current in transitions
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "yellow") "red")
(check-error (find fsm-traffic "black")
             "not found: black")
(define (find transitions current)
  (cond [(empty? transitions) (error "not found: " current)]
        [(cons? transitions)
         (if (state=? (transition-current (first transitions)) current)
             (transition-next (first transitions))
             (find (rest transitions) current))]))
