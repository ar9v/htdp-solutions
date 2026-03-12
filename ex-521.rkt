#lang htdp/isl+

(require 2htdp/image)

;;; Develop a representation for the states of the missionary-and-cannibal puzzle. Like
;;; the graphical representation, a data representation must record the number of
;;; missionaries and cannibals on each side of the river plus the location of the boat.

(define-struct bank [missionaries cannibals])
; a Bank is a structure
;   (make-bank N N)
;
; interpretation: (make-bank m c) represents a river bank that has `m` missionaries and
; `c` cannibals.

; a Place is one of
; -- 'left
; -- 'right

(define-struct puzzle-state [left boat-place right])
; a PuzzleState is a structure
;   (make-puzzle-state Bank Place Bank)
;
; interpretation: (make-puzzle-state l d r) represents a state in which `l` is the
; state of the left river bank, `d` is the place where the boat is, and `r` is the
; state of the right river bank
;
; constraint: the sum of missionaries and cannibals in both banks should be
; 2 * PEOPLE-TOTAL, where PEOPLE-TOTAL is the amount of each type of person (e.g.
; PEOPLE-TOTAL = 3 means there are three cannibals and three missionaries)
(define PEOPLE-TOTAL 3)

;;; The description of `PuzzleState` calls for a new structure type. Represent the above
;;; initial, intermediate, and final states in your representation.
(define full-bank (make-bank PEOPLE-TOTAL PEOPLE-TOTAL))
(define empty-bank (make-bank 0 0))

(define initial-state
  (make-puzzle-state
   full-bank
   'left
   empty-bank))

(define final-state
  (make-puzzle-state
   empty-bank
   'right
   full-bank))

(define intermediate-state
  (make-puzzle-state
   (make-bank PEOPLE-TOTAL (- PEOPLE-TOTAL 1))
   'right
   (make-bank 0 1)))

(define undesirable-state
  (make-puzzle-state
   (make-bank (- PEOPLE-TOTAL 1) PEOPLE-TOTAL)
   'right
   (make-bank 1 0)))


;;; Design the function `final?`, which detects whether in a given state all people are
;;; on the right river bank.

; final?: PuzzleState -> Boolean
; true if `state` is the final state
(check-expect (final? initial-state) #false)
(check-expect (final? intermediate-state) #false)
(check-expect (final? undesirable-state) #false)
(check-expect (final? final-state) #true)
(define (final? state)
  (equal? state final-state))


;;; Design the function `render-mc`, which maps a state of the missionary-and-cannibal
;;; puzzle to an image.
(define PERSON-RADIUS 10)
(define PERSON-WIDTH (* 2 PERSON-RADIUS))

(define BANK-WIDTH (* 4 PERSON-WIDTH))
(define BANK-HEIGHT (* 2 BANK-WIDTH))

(define MISSIONARY (circle PERSON-RADIUS 'solid 'black))
(define CANNIBAL (circle PERSON-RADIUS 'solid 'white))
(define BANK (rectangle BANK-WIDTH BANK-HEIGHT 'solid 'lightbrown))
(define RIVER (rectangle (* 2 BANK-WIDTH) BANK-HEIGHT 'solid 'lightblue))
(define BOAT (rectangle (* 3 PERSON-WIDTH) (* 2 PERSON-WIDTH) 'solid 'brown))

; render-mc: PuzzleState -> Image
; Represents `state` graphically
(check-expect (render-mc initial-state)
              (beside
               (render-bank (puzzle-state-left initial-state))
               (render-boat-place (puzzle-state-boat-place initial-state))
               (render-bank (puzzle-state-right initial-state))))
(define (render-mc state)
  (beside
   (render-bank (puzzle-state-left state))
   (render-boat-place (puzzle-state-boat-place state))
   (render-bank (puzzle-state-right state))))

; render-bank: Bank -> Image
; Represents Bank `b` graphically
(check-expect (render-bank (make-bank 2 1))
              (overlay/align
               'center 'center
               (render-people 2 1)
               BANK))
(define (render-bank b)
  (overlay/align
   'center 'center
   (render-people (bank-missionaries b) (bank-cannibals b))
   BANK))

; render-people: N N -> Image
; Renders `m` missionaries and `c` cannibals in two columns, side by side
; (circle PERSON-RADIUS 'outline 'black)
(check-expect (render-people 3 2)
              (beside/align
               'top
               (above/list (make-list 3 MISSIONARY))
               (above/list (make-list 2 CANNIBAL))))
(define (render-people m c)
  (beside/align
   'top
   (above/list (make-list m MISSIONARY))
   (above/list (make-list c CANNIBAL))))

; render-boat-place: Place -> Image
; Represents Place `place` graphically
(check-expect (render-boat-place 'right)
              (overlay/align 'right 'center BOAT RIVER))
(check-expect (render-boat-place 'left)
              (overlay/align 'left 'center BOAT RIVER))
(define (render-boat-place place)
  (overlay/align place 'center BOAT RIVER))

; above/list: [Image] -> Image
; Like 2htdp/image's `above`, but takes in a list
(check-expect (above/list '()) empty-image)
(check-expect (above/list (make-list 3 (square 5 'solid 'red)))
              (above (square 5 'solid 'red)
                     (above (square 5 'solid 'red)
                            (above (square 5 'solid 'red)
                                   empty-image))))
(define (above/list imgs) (foldr above empty-image imgs))
