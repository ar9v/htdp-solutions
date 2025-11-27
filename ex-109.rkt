#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Design a world program that recognizes a pattern in a sequence of KeyEvents. Initially
;;; the program shows a 100 by 100 white rectangle. Once your program has encountered its
;;; first desired letter, it displays a yellow rectangle of the same size. After
;;; encountering the final letter, the color of the rectangle turns green. If any
;;; "bad" key event occurs, the program displays a red rectangle.

;;; The specific sequences that your program looks for start with "a", followed by
;;; an arbitrarily long mix of "b" and "c", and ended by a "d". Clearly, "acbd" is one
;;; example of an acceptable string; two others are "ad" and "abcbbbcd". Of course,
;;; "da", "aa", or "d" do not match.


; Constants
(define WIDTH 100)

; Graphical Constants
(define WHITE-SQUARE (square WIDTH "solid" "white"))
(define YELLOW-SQUARE (square WIDTH "solid" "yellow"))
(define GREEN-SQUARE (square WIDTH "solid" "green"))
(define RED-SQUARE (square WIDTH "solid" "red"))


; REState is one of
; -- A
; -- BC
; -- D
; -- ERR
(define A "START")
(define BC "BC")
(define D "END")
(define ERR "ERROR")


; recognize: REState -> REState
; Kicks off the Regexp recognition program
(define (recognize re-state)
  (big-bang re-state
            [to-draw render]
            [on-key transition]
            [stop-when end? render]))

; render: REState -> Image
; Renders a rectangle according to the current state of our pattern recognition. If
; we haven't typed anything, render a WHITE-SQUARE. If we've typed
; something valid, we render YELLOW-SQUARE. If we're done, GREEN-SQUARE, and if we're
; wrong, RED-SQUARE
(check-expect (render A) WHITE-SQUARE)
(check-expect (render BC) YELLOW-SQUARE)
(check-expect (render D) GREEN-SQUARE)
(check-expect (render ERR) RED-SQUARE)
(define (render re-state)
  (cond [(equal? re-state A) WHITE-SQUARE]
        [(equal? re-state BC) YELLOW-SQUARE]
        [(equal? re-state D) GREEN-SQUARE]
        [(equal? re-state ERR) RED-SQUARE]))

; transition: REState KeyEvent -> REState
; Given KeyEvent `ke`, transition our FSM state
(check-expect (transition A "a") BC)
(check-expect (transition A "q") ERR)
(check-expect (transition BC "b") BC)
(check-expect (transition BC "c") BC)
(check-expect (transition BC "d") D)
(check-expect (transition BC "q") ERR)
(define (transition re-state ke)
  (cond [(equal? re-state A) (if (key=? ke "a") BC ERR)]
        [(equal? re-state BC)
         (cond [(or (key=? ke "b") (key=? ke "c")) BC]
               [(key=? ke "d") D]
               [else ERR])]))

; end?: REState -> Boolean
; End when we're either in an ERR state or D state
(check-expect (end? A) #false)
(check-expect (end? BC) #false)
(check-expect (end? D) #true)
(check-expect (end? ERR) #true)
(define (end? re-state)
  (or (equal? re-state D) (equal? re-state ERR)))

;;; HINT: Your solution implements a finite state machine (FSM), an idea introduced in
;;; chapter 4.7 as one design principle behind world programs. As the name says, an FSM
;;; program may be in one of a finite number of states. The first state is called an
;;; _initial state_. Each key event causes the machine to reconsider its current state;
;;; it may transition to the same state or another one. When your program recognizes a
;;; proper sequence of key events, it transitions to a _final state_.
;;;
;;; For a sequence-recognition problem, states typically represent the letters that the
;;; machine expects to see next; see Figure 36 for a data definition. Take a look at
;;; the last state, which says an illegal input has been encountered. Figure 37 shows
;;; how to think of these states and their relationships in a diagrammatic manner. Each
;;; node corresponds to one of the four finite states; each arrow specifies which
;;; KeyEvent causes the program to transition from one state to another.


;;; HISTORY: In the 1950s, Stephen C. Kleene, whom we would call a computer scientist,
;;; invented _regular expressions_ as a notation for the problem of recognizing text
;;; patterns. For the above problem, Kleene would write:
;;;
;;; a (b|c)* d
;;;
;;; which means a followed by b or c arbitrarily often until d is encountered.
