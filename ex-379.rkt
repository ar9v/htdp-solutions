#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Formulate test cases for `find`

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;  (cons FSM-State (cons FSM-State '()))
;
; An FSM-State is a String that specifies a color

(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))

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
               (find transitions current))]))

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
