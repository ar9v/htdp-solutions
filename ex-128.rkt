#lang htdp/bsl

;;; Copy the following tests into DrRacket's definitions area. Validate that all of them
;;; fail and explain why.

;;; This fails because "green" is not a member of ["red", "yellow", "grey"]
(check-member-of "green" "red" "yellow" "grey")

;;; This fails because
;;; - for x: #i1.0 - #0.9 = ~ 0.09, which is greater than 0.01, the test delta
;;; - for y: #i1.1 - 1.2 = ~ -0.09, which is greater than 0.01, the test delta
(check-within (make-posn #i1.0 #i1.1)
              (make-posn #i0.9 #i1.2) 0.01)

;;; This fails because #i0.9 is not in [#i0.6, #i0.8]
(check-range #i0.9 #i0.6 #i0.8)

;;; This fails because the posns wind up with different numbers. To ensure that
;;; `check-random` works, the expected and actual value expressions must call
;;; `random` with the same argument, in the same order.
(check-random (make-posn (random 3) (random 9))
              (make-posn (random 9) (random 3)))

;;; This fails because 4 is `even?`
(check-satisfied 4 odd?)
