#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Reformulate the data definition for 1Transition so that it is possible to restrict
;;; certain transitions to certain keystrokes. Try to formulate the change so that `find`
;;; continues to work without change. What else do you need to change to get the
;;; complete program to work? Which part of the design recipe provides the answer(s)?
;;;
;;; See exercise 229 for the original exercise statement.

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;  (cons (cons FSM-State (cons KeyEvent '()))
;        (cons FSM-State '()))
;
; An FSM-State is a String that specifies a color

(define fsm-traffic
  '((("red" "g") "green") (("green" "y") "yellow") (("yellow" "r") "red")))

(define SIZE 100)
(define FONT-SIZE (floor (/ SIZE 4)))

; simulate: FSM FSM-State -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
            [to-draw
             (lambda (current)
               (overlay
                (text current FONT-SIZE 'black)
                (square 100 "solid" current)))]
            [on-key
             (lambda (current key-event)
               (find transitions (list current key-event)))]))

; find: [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(check-expect (find '((1 2) (3 4) (5 6)) 3) 4)
(check-expect (find '((color "green") (size "sm") (display "inline")) 'display)
              "inline")
(check-error (find '() "key") "not found")
(check-error (find '(("Robert" 3) ("Richard" 2) ("Matt" 1)) "Bob")
             "not found")
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))
