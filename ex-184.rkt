#lang htdp/bsl+

;;; Determine the values of the following expressions. Use `check-expect` to express your
;;; answers.

;;; 1.
(check-expect (list (string=? "a" "b") #false) (list #false #false))

;;; 2.
(check-expect
 (list (+ 10 20) (* 10 20) (/ 10 20))
 (list 30 200 1/2))

;;; 3.
(check-expect
 (list "dana" "jane" "mary" "laura")
 (cons "dana" (cons "jane" (cons "mary" (cons "laura" '())))))
