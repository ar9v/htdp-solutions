#lang htdp/isl+

(require 2htdp/abstraction)
(require 2htdp/image)
(require 2htdp/universe)

;;; Run the code in figure 130 with the BW Machine configuration from exercise 382.

; <machine initial="white">
;   <action state="white" next="black" />
;   <action state="black" next="white" />
; </machine>
(define bw-machine
  '(machine ((initial "white"))
            (action ((state "white") (next "black")))
            (action ((state "black") (next "white")))))

(define traffic-machine
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))

(define fsm-traffic '(("red" "green") ("green" "yellow") ("yellow" "red")))
(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))

(define SIZE 100)

; simulate-xmachine: XMachine -> FSM-State
; interprets the given configuration as a state machine
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))

; simulate: FSM FSM-State -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
            [to-draw (lambda (current) (square SIZE "solid" current))]
            [on-key (lambda (current _key-event) (find transitions current))]))

; xm-state: XMachine -> FSM-State
; extracts and translates the transition table from xm0
(check-expect (xm-state0 xm0) "red")
(define (xm-state0 xm0)
  (find (xexpr-attr xm0) 'initial))

; xm->transitions: XMachine -> [List-of 1Transition]
; extracts the transition table from xm
(check-expect (xm->transitions xm0) fsm-traffic)
(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find (xexpr-attr xa) 'state)
                  (find (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))

; xexpr-attr: XExpr -> [List-of Attribute]
; extracts the list of attributes of a given Xexpr `xe`
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

; xexpr-content: Xexpr -> Body
; Produces the body of `xe`, meaning, the nested Xexprs within it
(define (xexpr-content xe)
  (match xe
    [(list (? symbol?)) '()]
    [(list (? symbol?) (? list-of-attributes?)) '()]
    [(cons (? symbol?) (cons (? list-of-attributes?) (cons x xs))) (cons x xs)]
    [(cons (? symbol?) (cons x xs)) (cons x xs)]
    [other (error "Error: " other " is not a valid XML representation")]))

; list-of-attributes? [[List-of Attribute] | Xexpr] -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (or (empty? x) (cons? (first x))))

; find: [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))
