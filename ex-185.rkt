#lang htdp/bsl+

;;; You know about `first` and `rest` from BSL, but BSL+ comes with even more selectors
;;; than that. Determine the values of the following expressions:

;;; 1.
(check-expect
 (first (list 1 2 3))
 1)

;;; 2.
(check-expect
 (rest (list 1 2 3))
 (list 2 3))

;;; 3.
(check-expect
 (second (list 1 2 3))
 2)

;;; Find out from the documentation whether `third` and `fourth` exist.

;;; A:
;;;
;;; They do (as do fifth, sixth, seventh...)
