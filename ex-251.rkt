#lang htdp/isl

;;; Design `fold1`, which is the abstraction of the two functions in figure 93.

; fold1: [Number Number -> Number] Number [List-of Number] -> Number
; Produces the result of applying `f` to successive elements of `l`, from right to left.
(check-expect (fold1 + 0 '()) 0)
(check-expect (fold1 + 0 (list 1 2 3)) 6)
(check-expect (fold1 * 1 (list 1 2 3)) 6)
(define (fold1 f n0 l)
  (cond [(empty? l) n0]
        [(cons? l) (f (first l) (fold1 f n0 (rest l)))]))
