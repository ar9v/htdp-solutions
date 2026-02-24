#lang htdp/isl+

(require 2htdp/abstraction)

;;; The key idea to solving the n-queens problem is to design a function that places `n`
;;; queens on a chess board that may already contain some queens:

; place-queens: Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise returns #false
(define (place-queens a-board n)
  (cond [(zero? n) '()]
        [else (for/or [(p (find-open-spots a-board))]
                (local [(define candidate (place-queens (add-queen a-board p) (sub1 n)))]
                  (if (list? candidate) (cons p candidate) #false)))]))

;;; Figure 175 already refers to this function in the definition of `n-queens`.

;;; Design the `place-queens` algorithm. Assume you have the following functions to deal
;;; with Boards:

; board0: N -> Board
; creates the initial n by n board
(define (board0 n)
  '())

; add-queen: Board QP -> Board
; places a queen at `qp` on `a-board`
(define (add-queen a-board qp)
  a-board)

; find-open-spots: Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots a-board)
  '())

;;; The first function is used in figure 175 to create the initial board representation for
;;; `place-queens`. You will need the other two to describe the generative steps for the
;;; algorithm.
