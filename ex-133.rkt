#lang htdp/bsl

;;; Here is another way of formulating the second `cond` clause in `contains-flatt?`
;;;
;;; ... (cond [(string=? (first alon) "Flatt") #true]
;;;           [else (contains-flatt? (rest alon))]) ...
;;;
;;; Explain why this expression produces the same answers as the `or` expression in the
;;; version of figure 47. Which version is better? Explain.

;;; A:
;;;
;;; In Intermezzo 1, we learned that
;;;
;;; (or exp-1 exp-2)
;;; ===
;;; (cond [exp-1 #true] [else exp-2])
;;;
;;; The alternative this exercise suggests is merely applying that transformation.
;;;
;;; Since they are both equivalent, one is not better than other; it just might be that
;;; the original `or` expression reads a bit more cleanly (e.g. having a cond clause
;;; result in a boolean is somewhat redundant).
