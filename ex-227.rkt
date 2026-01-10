#lang htdp/bsl+

(require 2htdp/image)

;;; The BW Machine is an FSM that flips from black to white and back to black for every
;;; key event. Formulate a data representation for the BW Machine.

; An FSM is one of:
; -- '()
; -- (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

; A FSM-State is a Color

(define bw-machine
  (list (make-transition "black" "white") (make-transition "white" "black")))
