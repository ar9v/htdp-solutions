#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design the function `create-bst-from-list`. It consumes a list of numbers and names and
;;; produces a BST by repeatedly applying `create-bst`. Here is the signature:
;;;
;;; create-bst-from-list: [List-of [List Number Symbol]] -> BST

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
    (make-node 15 'd
     (make-node 10 'h NONE NONE)
     (make-node 24 'i NONE NONE))
    NONE)
   (make-node 89 'c
    (make-node 77 'l NONE NONE)
    (make-node 95 'g NONE (make-node 99 'o NONE NONE)))))

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
;;; Use the complete function to create a BST from this sample input:

(define sample-input
  '((99 o)
    (77 l)
    (24 i)
    (10 h)
    (95 g)
    (15 d)
    (89 c)
    (29 b)
    (63 a)))

; create-bst-from-list: [List-of [List Number Symbol]] -> BST
(define (create-bst-from-list l)
  (match l
    ['() NONE]
    [(cons (list n s) rst) (create-bst (create-bst-from-list rst) n s)]))

;;; The result is tree A in figure 119, if you follow the structural design recipe. If
;;; you use an existing abstraction, you may still get this tree, but you may also get an
;;; "inverted" one. Why?
;;;
;;; Because we can use either `foldr` or `foldl` to define the function, and `create-bst`
;;; is not associative (e.g. it is not the same to first insert 99 and then 63 than to
;;; insert 63 and then 99; the data invariant is preserved by `create-bst`, but the
;;; shape is not guaranteed to be the same)
(check-expect (create-bst-from-list sample-input) bstA)

; create-bst-from-list.foldr: [List-of [List Number Symbol]] -> BST
(check-expect (create-bst-from-list.foldr sample-input) bstA)
(define (create-bst-from-list.foldr l)
  (foldr (λ (pair bst) (create-bst bst (first pair) (second pair)))
         NONE l))

; create-bst-from-list.foldl: [List-of [List Number Symbol]] -> BST
(check-expect (create-bst-from-list.foldl sample-input) bstA)
(define (create-bst-from-list.foldl l)
  (foldl (λ (pair bst) (create-bst bst (first pair) (second pair)))
         NONE (reverse l))) ; notice `reverse` to make the test pass
