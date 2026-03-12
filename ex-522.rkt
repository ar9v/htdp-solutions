#lang htdp/isl+

(require 2htdp/image)

;;; Modify the representation from exercise 521 so that a state records the sequence of
;;; states traversed to get there. Use a list of states.

(define-struct bank [missionaries cannibals])
; a Bank is a structure
;   (make-bank N N)
;
; interpretation: (make-bank m c) represents a river bank that has `m` missionaries and
; `c` cannibals.

; a Place is one of
; -- 'left
; -- 'right

(define-struct puzzle-state [left boat-place right visited])
; a PuzzleState is a structure
;   (make-puzzle-state Bank Place Bank [List-of PuzzleState])
;
; interpretation: (make-puzzle-state l d r v) represents a state in which `l` is the
; state of the left river bank, `d` is the place where the boat is, and `r` is the
; state of the right river bank. `v` is the PuzzleStates we've gone through to reach
; this state.
;
; constraint: the sum of missionaries and cannibals in both banks should be
; 2 * PEOPLE-TOTAL, where PEOPLE-TOTAL is the amount of each type of person (e.g.
; PEOPLE-TOTAL = 3 means there are three cannibals and three missionaries)
(define PEOPLE-TOTAL 3)

(define full-bank (make-bank PEOPLE-TOTAL PEOPLE-TOTAL))
(define empty-bank (make-bank 0 0))

(define initial-state
  (make-puzzle-state
   full-bank
   'left
   empty-bank
   '()))

(define final-state
  (make-puzzle-state
   empty-bank
   'right
   full-bank
   '()))

(define intermediate-state
  (make-puzzle-state
   (make-bank PEOPLE-TOTAL (- PEOPLE-TOTAL 1))
   'right
   (make-bank 0 1)
   (list initial-state)))

(define undesirable-state
  (make-puzzle-state
   (make-bank (- PEOPLE-TOTAL 1) PEOPLE-TOTAL)
   'right
   (make-bank 1 0)
   (list initial-state)))

;;; Articulate and write down an accumulator statement with the data definition that
;;; explains the additional field.
;;;
;;; A:
;;; accumulator visited: The sequence of `PuzzleState`s that result in the state given.


;;; Modify `final?` or `render-mc` for this representation as needed.

; final?: PuzzleState -> Boolean
; true if `state` is the final state
;
; NOTE: changed so that we can avoid caring about the accumulator
(check-expect (final? initial-state) #false)
(check-expect (final? intermediate-state) #false)
(check-expect (final? undesirable-state) #false)
(check-expect (final? final-state) #true)
(define (final? state)
  (and (equal? (puzzle-state-left state) empty-bank)
       (equal? (puzzle-state-boat-place state) 'right)
       (equal? (puzzle-state-right state) full-bank)))

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
;
; NOTE: No changes needed here, reproduced for convenience
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
