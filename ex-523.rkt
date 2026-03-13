#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design the `create-next-states` function. It consumes lists of missionary-and-cannibal
;;; states and generates the list of all those states that a boat ride can reach.
;;;
;;; Ignore the accumulator in the first draft of `create-next-states`, but make sure that
;;; the function does not generate states where the cannibals can eat the missionaries.
;;;
;;; For the second design, update the accumulator field in the state structures and use it
;;; to rule out states that have been encountered on the way to the current state.

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
(define PEOPLE-PER-TRIP 2)

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

(define intermediate-state-1
  (make-puzzle-state
   (make-bank PEOPLE-TOTAL (- PEOPLE-TOTAL 1))
   'right
   (make-bank 0 1)
   (list initial-state)))

(define intermediate-state-2
  (make-puzzle-state
   (make-bank PEOPLE-TOTAL (- PEOPLE-TOTAL 2))
   'right
   (make-bank 0 2)
   (list initial-state)))

(define intermediate-state-3
  (make-puzzle-state
   (make-bank (- PEOPLE-TOTAL 1) (- PEOPLE-TOTAL 1))
   'right
   (make-bank 1 1)
   (list initial-state)))

(define intermediate-state-4
  (make-puzzle-state
   (make-bank (- PEOPLE-TOTAL 2) PEOPLE-TOTAL)
   'right
   (make-bank 2 0)
   (list initial-state)))

(define intermediate-state-5
  (make-puzzle-state
   (make-bank (- PEOPLE-TOTAL 1) PEOPLE-TOTAL)
   'right
   (make-bank 1 0)
   (list initial-state)))

(define intermediate-state-3-3
  (make-puzzle-state
   (make-bank 3 2) 'left (make-bank 0 1) (list intermediate-state-3 initial-state)))

(define second-layer
  (list intermediate-state-1
        intermediate-state-2
        intermediate-state-3))

(define third-layer (list intermediate-state-3-3))

; create-next-states: [List-of PuzzleState] -> [List-of PuzzleState]
; Produces the states we can get to from the list `los`
(check-expect (create-next-states (list initial-state)) second-layer)
(check-expect (create-next-states (list intermediate-state-3)) third-layer)
(define (create-next-states los)
  (local [(define (create-next-states/state s)
            (local [(define bank (current-bank s))
                    (define ms (bank-missionaries bank))
                    (define cs (bank-cannibals bank))
                    (define possible-ms (range 0 (add1 (min ms PEOPLE-PER-TRIP)) 1))
                    (define possible-cs (range 0 (add1 (min cs PEOPLE-PER-TRIP)) 1))
                    (define visited (puzzle-state-visited s))
                    (define (same? s) (λ (other) (puzzle-state-equal? s other)))
                    (define (visited? s) (not (false? (find (same? s) visited))))
                    (define preds (list undesirable? visited? (same? s)))]
              (reject (any-pred preds)
                      (map (λ (pair) (cross s (first pair) (second pair)))
                           (filter
                            (λ (pair) (<= (+ (first pair) (second pair)) PEOPLE-PER-TRIP))
                            (product possible-ms possible-cs))))))]
    (foldr (λ (s gen-d) (append (create-next-states/state s) gen-d)) '() los)))

; cross: PuzzleState N N -> PuzzleState
; Returns the puzzle state that results of crossing `m` missionaries and `c` cannibals
; to the bank opposite of `state`'s current `boat-place`
;
; assumptions:
;   - m + c <= PEOPLE-PER-TRIP
;   - m <= (bank-missionaries (current-bank state))
;   - c <= (bank-cannibals (current-bank state))
(check-expect (cross initial-state 0 0) initial-state)
(check-expect (cross initial-state 0 1) intermediate-state-1)
(define (cross state m c)
  (local [(define is-left? (equal? (puzzle-state-boat-place state) 'left))
          (define lbank-op (if is-left? - +))
          (define rbank-op (if is-left? + -))
          (define new-place (if is-left? 'right 'left))
          (define lbank-ms (bank-missionaries (puzzle-state-left state)))
          (define lbank-cs (bank-cannibals (puzzle-state-left state)))
          (define rbank-ms (bank-missionaries (puzzle-state-right state)))
          (define rbank-cs (bank-cannibals (puzzle-state-right state)))]
    (if (and (zero? m) (zero? c))
        state
        (make-puzzle-state (make-bank (lbank-op lbank-ms m) (lbank-op lbank-cs c))
                           new-place
                           (make-bank (rbank-op rbank-ms m) (rbank-op rbank-cs c))
                           (cons state (puzzle-state-visited state))))))

; undesirable?: PuzzleState -> Boolean
; True if `state` has a bank with more cannibals than missionaries
(check-expect (undesirable? initial-state) #false)
(check-expect (undesirable? intermediate-state-1) #false)
(check-expect (undesirable? intermediate-state-4) #true)
(check-expect (undesirable? intermediate-state-5) #true)
(define (undesirable? state)
  (local [(define lbank (puzzle-state-left state))
          (define rbank (puzzle-state-right state))
          (define lms (bank-missionaries lbank))
          (define lcs (bank-cannibals lbank))
          (define rms (bank-missionaries rbank))
          (define rcs (bank-cannibals rbank))]
    (or (and (positive? lms) (< lms lcs))
        (and (positive? rms) (< rms rcs)))))

; puzzle-state-equal? PuzzleState PuzzleState -> Boolean
; True if both states have the same banks and boat places
(check-expect (puzzle-state-equal? initial-state final-state) #false)
(check-expect (puzzle-state-equal? initial-state initial-state) #true)
(check-expect (puzzle-state-equal? initial-state
                                   (make-puzzle-state
                                    (make-bank PEOPLE-TOTAL PEOPLE-TOTAL)
                                    'left
                                    (make-bank 0 0)
                                    '(foo bar baz))) #true)
(define (puzzle-state-equal? s1 s2)
  (and (equal? (puzzle-state-left s1) (puzzle-state-left s2))
       (equal? (puzzle-state-right s1) (puzzle-state-right s2))
       (equal? (puzzle-state-boat-place s1) (puzzle-state-boat-place s2))))

; current-bank: PuzzleState -> Bank
; Where are we?
(check-expect (current-bank initial-state) (puzzle-state-left initial-state))
(check-expect (current-bank final-state) (puzzle-state-right final-state))
(define (current-bank state)
  (local [(define place (puzzle-state-boat-place state))]
    (if (equal? place 'left)
        (puzzle-state-left state)
        (puzzle-state-right state))))

; product: [List-of Symbol] [List-of Number] -> [List-of (list Symbol Number)]
; Computes the cartesian product of `ss` x `ns`
(check-expect (product '() '(1 2)) '())
(check-expect (product '(a b c) '()) '())
(check-expect (product '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(define (product ss ns)
  (foldr (λ (s ps) (append (map (λ (n) (list s n)) ns) ps)) '() ss))

; find: [Any -> Boolean] [List-of Any] -> [Maybe Any]
; returns the first element in `l` that fulfills `pred`
(define (find pred l) (for/or [(e l)] (if (pred e) e #false)))

; any-pred: [List-of [X -> Boolean]] -> [X -> Boolean]
; Given `preds` returns a predicate for that'll return true if `x` fulfills any predicate
(check-expect (filter (any-pred (list even? negative?)) '(-1 0 1 2 3)) '(-1 0 2))
(define (any-pred preds)
  (λ (x) (for/or [(p preds)] (p x))))

; reject: [X -> Boolean] [List-of X] -> [List-of X]
; The complement to filter
(check-expect (reject even? '(0 1 2 3 4)) '(1 3))
(define (reject pred xs)
  (foldr (λ (x res) (if (pred x) res (cons x res))) '() xs))
