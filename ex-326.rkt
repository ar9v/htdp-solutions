#lang htdp/isl+

;;; Design the function `create-bst`. It consumes a BST `B`, a number `N` and a symbol `S`.
;;; It produces a BST that is just like `B` and that in place of one NONE subtree contains
;;; the `node` structure
;;;
;;; (make-node N S NONE NONE)
;;;
;;; Once the design is completed, use the function on tree A from figure 119.

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

; create-bst: BST N Symbol -> BST
; Create a new BST that is `B` with a new node with `N` and `S` added to it. If a node
; with `ssn` `N` exists, updates its symbol to `S`.
(check-expect (create-bst NONE 16 'z) (make-node 16 'z NONE NONE))
(check-expect (create-bst bst1 14 'z)
              (make-node 15 'd
               (make-node 14 'z NONE NONE)
               (make-node 24 'i NONE NONE)))
(check-expect (create-bst bst1 16 'z)
              (make-node 15 'd
               NONE
               (make-node 24 'i
                (make-node 16 'z NONE NONE)
                NONE)))
(check-expect (create-bst bst1 25 'z)
              (make-node 15 'd
               NONE
               (make-node 24 'i
                NONE
                (make-node 25 'z NONE NONE))))
(check-expect (create-bst bst1 15 'z)
              (make-node 15 'z
               NONE
               (make-node 24 'i
                NONE
                NONE)))
(define (create-bst B N S)
  (cond [(equal? B NONE) (make-node N S NONE NONE)]
        [else
         (cond [(= (node-ssn B) N) (make-node N S (node-left B) (node-right B))]
               [(< (node-ssn B) N)
                (make-node (node-ssn B)
                        (node-name B)
                        (node-left B)
                        (create-bst (node-right B) N S))]
               [else
                (make-node (node-ssn B)
                        (node-name B)
                        (create-bst (node-left B) N S)
                        (node-right B))])]))
