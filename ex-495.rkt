#lang htdp/isl+

;;; Complete the manual evaluation of `(sum/a '(10 4) 0)` in figure 183.

;; (sum.v2 '(10 4))
;; ==
;; (sum/a '(10 4) 0)
;; ==
;; (sum/a '(4) (+ 0 10))
;; ==
;; (sum/a '() (+ 4 10))
;; ==
;; 14

;;; Doing so shows that the `sum` and `sum.v2` add up the given numbers in reverse order.
;;; While `sum` adds up the numbers from right to left, the accumulator-style version
;;; adds them from left to right.
;;;
;;; NOTE ON NUMBERS:
;;; Remember that for exact numbers, this difference has no effect on the final result.
;;; For inexact numbers, the difference can be significant. See the exercises at the
;;; end of intermezzo 5.
