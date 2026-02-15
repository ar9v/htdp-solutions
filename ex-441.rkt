#lang htdp/isl+

;;; Evaluate
;;;
;;; (quick-sort< (list 10 6 8 9 14 12 3 11 14 16 2))
;;;
;;; by hand. Show only those lines that introduce a new recursive call to `quick-sort<`.
;;; How many recursive applications of `quick-sort<` are required? How many recursive
;;; applications of the `append` function? Suggest a general rule for a list of length
;;; `n`

;; (quick-sort< (list 10 6 8 9 14 12 3 11 14 16 2))
;; ==
;; (append (quick-sort< '(6 8 9 3 2))
;;         '(10)
;;         (quick-sort< '(14 12 11 14 16)))
;; ==
;; (append (append (quick-sort< '(3 2))
;;                 '(6)
;;                 (quick-sort< '(8 9)))
;;         '(10)
;;         (quick-sort< '(14 12 11 14 16)))
;; ==
;; (append (append (append (quick-sort< '(2))
;;                         '(3)
;;                         (quick-sort< '()))
;;                 '(6)
;;                 (quick-sort< '(8 9)))
;;         '(10)
;;         (quick-sort< '(14 12 11 14 16)))
;; ==
;; (append (append (append (append (quick-sort< '())
;;                                 '(2)
;;                                 (quick-sort< '()))
;;                         '(3)
;;                         (quick-sort< '()))
;;                 '(6)
;;                 (quick-sort< '(8 9)))
;;         '(10)
;;         (quick-sort< '(14 12 11 14 16)))
;; ==
;; (append (append (append (append (quick-sort< '())
;;                                 '(2)
;;                                 (quick-sort< '()))
;;                         '(3)
;;                         (quick-sort< '()))
;;                 '(6)
;;                 (append (quick-sort< '(8))
;;                         '(9)
;;                         (quick-sort< '())))
;;         '(10)
;;         (quick-sort< '(14 12 11 14 16)))
;; ==
;; (append (append (append (append (quick-sort< '())
;;                                 '(2)
;;                                 (quick-sort< '()))
;;                         '(3)
;;                         (quick-sort< '()))
;;                 '(6)
;;                 (append (append (quick-sort< '())
;;                                 '(8)
;;                                 (quick-sort< '()))
;;                         '(9)
;;                         (quick-sort< '())))
;;         '(10)
;;         (quick-sort< '(14 12 11 14 16)))
;; ==
;; (append (append (append (append (quick-sort< '())
;;                                 '(2)
;;                                 (quick-sort< '()))
;;                         '(3)
;;                         (quick-sort< '()))
;;                 '(6)
;;                 (append (append (quick-sort< '())
;;                                 '(8)
;;                                 (quick-sort< '()))
;;                         '(9)
;;                         (quick-sort< '())))
;;         '(10)
;;         (append (quick-sort< '(12 11 14))
;;                 '(14)
;;                 (quick-sort< '(16))))
;; ==
;; (append (append (append (append (quick-sort< '())
;;                                 '(2)
;;                                 (quick-sort< '()))
;;                         '(3)
;;                         (quick-sort< '()))
;;                 '(6)
;;                 (append (append (quick-sort< '())
;;                                 '(8)
;;                                 (quick-sort< '()))
;;                         '(9)
;;                         (quick-sort< '())))
;;         '(10)
;;         (append (append (quick-sort< '(11))
;;                         '(12)
;;                         (quick-sort< '(14)))
;;                 '(14)
;;                 (quick-sort< '(16))))
;; ==
;; (append (append (append (append (quick-sort< '())
;;                                 '(2)
;;                                 (quick-sort< '()))
;;                         '(3)
;;                         (quick-sort< '()))
;;                 '(6)
;;                 (append (append (quick-sort< '())
;;                                 '(8)
;;                                 (quick-sort< '()))
;;                         '(9)
;;                         (quick-sort< '())))
;;         '(10)
;;         (append (append (append (quick-sort< '())
;;                                 '(11)
;;                                 (quick-sort< '()))
;;                         '(12)
;;                         (quick-sort< '(14)))
;;                 '(14)
;;                 (quick-sort< '(16))))
;; ==
;; (append (append (append (append (quick-sort< '())
;;                                 '(2)
;;                                 (quick-sort< '()))
;;                         '(3)
;;                         (quick-sort< '()))
;;                 '(6)
;;                 (append (append (quick-sort< '())
;;                                 '(8)
;;                                 (quick-sort< '()))
;;                         '(9)
;;                         (quick-sort< '())))
;;         '(10)
;;         (append (append (append (quick-sort< '())
;;                                 '(11)
;;                                 (quick-sort< '()))
;;                         '(12)
;;                         (append (quick-sort< '())
;;                                 '(14)
;;                                 (quick-sort< '())))
;;                 '(14)
;;                 (quick-sort< '(16))))
;; ==
;; (append (append (append (append (quick-sort< '())
;;                                 '(2)
;;                                 (quick-sort< '()))
;;                         '(3)
;;                         (quick-sort< '()))
;;                 '(6)
;;                 (append (append (quick-sort< '())
;;                                 '(8)
;;                                 (quick-sort< '()))
;;                         '(9)
;;                         (quick-sort< '())))
;;         '(10)
;;         (append (append (append (quick-sort< '())
;;                                 '(11)
;;                                 (quick-sort< '()))
;;                         '(12)
;;                         (append (quick-sort< '())
;;                                 '(14)
;;                                 (quick-sort< '())))
;;                 '(14)
;;                 (append (quick-sort< '())
;;                         '(16)
;;                         (quick-sort< '()))))
;;
;; append: 11
;; quick-sort<: 11 + 12 = 23 (11 appends + 12 trivial)
;;
;; Potential rule: `quick-sort<` performs 2n + 1 recursive calls,
;; where `n` is the length of the list.


;;; Evaluate
;;;
;;; (quick-sort< (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
;;;
;;; by hand. How many recursive applications of `quick-sort<` are required? How many
;;; recursive applications of `append`? Does this contradict the first part of the
;;; exercise?

;; (quick-sort< (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
;; ==
;; (append (quick-sort< '())
;;         '(1)
;;         (quick-sort< '(2 3 4 5 6 7 8 9 10 11 12 13 14)))
;; ==
;; (append (quick-sort< '())
;;         '(1)
;;         (append (quick-sort< '())
;;                 '(2)
;;                 (quick-sort< '(3 4 5 6 7 8 9 10 11 12 13 14))))
;; ==
;; (append (quick-sort< '())
;;         '(1)
;;         (append (quick-sort< '())
;;                 '(2)
;;                 (append (quick-sort< '())
;;                         '(3)
;;                         (quick-sort< '(4 5 6 7 8 9 10 11 12 13 14)))))
;;
;; ...
;;
;; It becomes apparent that this will require 14 appends, and that
;; there will be 14 + 15 = 29 calls to `quick-sort<`
;;
;; So, the amount of calls is consistent for both cases.
;;
;; (Spoilers: but their time complexity isn't, see Intermezzo 5)
