#lang htdp/isl+

;;; Design `search-bst`. The function consumes a number `n` and a BST. If the tree contains
;;; a `node` whose `ssn` field is `n`, the function produces the value of the `name` field
;;; in that node. Otherwise, the function produces NONE. The function organization must
;;; exploit the BST invariant so that the function performs as few comparisons as
;;; necessary.

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; a BT (short for Binary Tree) is one of
; -- NONE
; -- (make-node Number Symbol BT BT)

(define bst1
  (make-node 15 'd NONE (make-node 24 'i NONE NONE)))

(define bstA
  (make-node 63 'a
   (make-node 29 'b
    (make-node 15 'c
     (make-node 10 'd NONE NONE)
     (make-node 24 'e NONE NONE))
    NONE)
   (make-node 89 'f
    (make-node 77 'g NONE NONE)
    (make-node 95 'h NONE (make-node 99 'i NONE NONE)))))

; search-bst: N BST -> [Symbol | NONE]
(check-expect (search-bst 10 bstA) 'd)
(check-expect (search-bst 89 bstA) 'f)
(check-expect (search-bst 44 bstA) NONE)
(define (search-bst n bst)
  (cond [(equal? bst NONE) NONE]
        [else
         (cond [(= (node-ssn bst) n) (node-name bst)]
               [(< (node-ssn bst) n) (search-bst n (node-right bst))]
               [else (search-bst n (node-left bst))])]))

;;; See exercise 189 for searching in sorted lists. Compare!
