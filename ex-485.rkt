#lang htdp/isl+

;;; A number tree is either a number or a pair of number trees. Design `sum-tree`, which
;;; determines the sum of the numbers in the tree. What is its abstract running time?
;;; What is an acceptable measure of the size of such a tree? What is the worst possible
;;; shape of the tree? What's the best possible shape?
;;;
;;; A:
;;; Its abstract running time is in the order of n steps, where n is the amount of nodes.
;;; Alternatively, we can say that it is in the order of 2^n steps, where n is the depth
;;; of the list/tree.
;;;
;;; In the worst case, we have a skewed tree: if we have three levels deep on one side,
;;; we're _guaranteed_ to have 2^n nodes. In the best case, the tree is balanced: so, e.g.
;;; if the tree is 2 levels deep, we know there can be _no more_ than 2^2 = 4 nodes.
;;;
;;; E.g.
;;; ((1 2) (3 4))


; NumberTree is either
;   -- Number
;   -- (list NumberTree NumberTree)

; sum-tree: NumberTree -> Number
; Returns the result of adding all numbers in `ntree`
(check-expect (sum-tree 1) 1)
(check-expect (sum-tree '(((1 2) 3) (4 (5 (6 7))))) 28)
(define (sum-tree ntree)
  (cond [(number? ntree) ntree]
        [else (+ (sum-tree (first ntree)) (sum-tree (second ntree)))]))
