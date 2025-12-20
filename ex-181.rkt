#lang htdp/bsl+

;;; Use `list` to construct the equivalent of these lists. Start by determining how many
;;; items each list and each nested list contains. Use `check-expect` to express your
;;; answers; this ensures that your abbreviations are really the same as the long-hand.

;;; 1.
(check-expect
 (cons "a" (cons "b" (cons "c" (cons "d" '()))))
 (list "a" "b" "c" "d"))

;;; 2.
(check-expect
 (cons (cons 1 (cons 2 '())) '())
 (list (list 1 2)))

;;; 3.
(check-expect
  (cons "a" (cons (cons 1 '()) (cons #false '())))
  (list "a" (list 1) #false))

;;; 4.
(check-expect
 (cons (cons "a" (cons 2 '())) (cons "hello" '()))
 (list (list "a" 2) "hello"))

;;; Bonus:
(check-expect
 (cons (cons 1 (cons 2 '()))
       (cons (cons 2 '())
             '()))
 (list (list 1 2)
       (list 2)))
