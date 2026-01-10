#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Here is a revised data definition for Transition

(define-struct ktransition [current ke next])
; A Transition is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)

;;; Represent the FSM from exercise 109 using lists of Transitions; ignore errors and
;;; final states.

(define fsm-traffic
  (list (make-ktransition "red" " " "green")
        (make-ktransition "green" " " "yellow")
        (make-ktransition "yellow" " " "red")))

(define bw-machine
  (list (make-ktransition "black" "w" "white")
        (make-ktransition "white" "b" "black")))

; FSM-States for the Regexp can be one of
; -- START
; -- A
; -- BC
; -- D
(define START "white")
(define A "yellow")
(define BC "yellow")
(define D "green")

; a(b|c)*d
(define a-bc-d
  (list (make-ktransition START "a" A)
        (make-ktransition A "b" BC)
        (make-ktransition A "c" BC)
        (make-ktransition BC "b" BC)
        (make-ktransition BC "c" BC)
        (make-ktransition BC "d" D)))


;;; Modify the design of `simulate` so that it deals with keystrokes in the appropriate
;;; manner now. Follow the design recipe, starting with the adaptation of the data
;;; examples.

(define-struct fs [fsm current])

; simulate: FSM FSM-State -> FS
; Runs the Finite State Machine simulation
(define (simulate an-fsm s0)
  (big-bang (make-fs an-fsm s0)
            [to-draw state-as-colored-square]
            [on-key find-next-state]))

; state-as-colored-square: FS -> Image
; Renders the current state as a colored square
(define (state-as-colored-square current-fs)
  (square 100 "solid" (fs-current current-fs)))

; find-next-state: FS KeyEvent -> FS
; Finds the next state based on `ke`. If no transition has a matching KeyEvent, the current
; one is returned (alternatively, we could signal an error)
(check-expect
 (find-next-state (make-fs fsm-traffic "red") "n")
 (make-fs fsm-traffic "red"))
(check-expect
 (find-next-state (make-fs fsm-traffic "red") " ")
 (make-fs fsm-traffic "green"))
(check-expect
 (find-next-state (make-fs fsm-traffic "green") " ")
 (make-fs fsm-traffic "yellow"))
(define (find-next-state current-fs ke)
  (make-fs
   (fs-fsm current-fs)
   (if (empty? (find/keys (fs-fsm current-fs) ke))
       (fs-current current-fs)
       (find (find/keys (fs-fsm current-fs) ke) (fs-current current-fs)))))

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

; find/keys: FSM KeyEvent -> FSM
; Finds the transitions in `fsm` that include `ke`
(define (find/keys fsm ke)
  (cond [(empty? fsm) '()]
        [(cons? fsm)
         (if (key=? ke (ktransition-ke (first fsm)))
             (cons (first fsm)
                   (find/keys (rest fsm) ke))
             (find/keys (rest fsm) ke))]))

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
         (if (state=? (ktransition-current (first transitions)) current)
             (ktransition-next (first transitions))
             (find (rest transitions) current))]))


;;; Use the revised program to simulate a run of the FSM from exercise 109 on the following
;;; sequence of keystrokes: "a", "b", "b", "c", and "d".
