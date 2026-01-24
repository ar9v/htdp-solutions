#lang htdp/isl+

;;; Design the function `inorder`. It consumes a binary tree and produces the sequence of
;;; all the `ssn` numbers in the tree as they show up from left to right when looking at
;;; a tree drawing.

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; a BT (short for Binary Tree) is one of
; -- NONE
; -- (make-node Number Symbol BT BT)

(define bt1
  (make-node
   15
   'd
   NONE
   (make-node 24 'i NONE NONE)))

(define treeA
  (make-node
   63 'a
   (make-node
    29 'b
    (make-node
     15 'c
     (make-node 10 'd NONE NONE)
     (make-node 24 'e NONE NONE))
    NONE)
   (make-node
    89 'f
    (make-node 77 'g NONE NONE)
    (make-node 95 'h NONE (make-node 99 'i NONE NONE)))))

; inorder: BT -> [List-of Number]
; Returns all ssn's in `bt` as they'd show up in a diagram, from left to right.
; (alternatively: returns ssn's after an inorder traversal)
(check-expect (inorder treeA) '(10 15 24 29 63 77 89 95 99))
(define (inorder bt)
  (cond [(equal? bt NONE) '()]
        [else
         (append (inorder (node-left bt))
                 (list (node-ssn bt))
                 (inorder (node-right bt)))]))

;;; What does `inorder` produce for a binary search tree?
;;;
;;; A: A sorted list of numbers! (or sorted list of items in a more general case)
