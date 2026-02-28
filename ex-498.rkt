#lang htdp/isl+

;; Complete height.v3.
;;
;; HINT:
;; The bottom-most tree of figure 184 contains no subtree to the left of the subtree marked
;; with 1. It contains one complete path from root to tree in the part of the tree that is
;; to the left of the subtree marked with 2; this path consists of two steps.

(define-struct node [left right])
; A Tree is one of:
; – '()
; – (make-node Tree Tree)

(define example
  (make-node (make-node '() (make-node '() '())) '()))

; height.v3: Tree -> Number
; Computes the height of `abt0`
(check-expect (height.v3 example) 3)
(define (height.v3 abt0)
  (local [; h/a: Tree N N -> N
          ; measures the height of `abt`
          ; accumulator s: the number of steps it takes to reach `abt` from `abt0`
          ; accumulator m: the maximal height of the part of `abt0` that is to the left
          ;                of `abt`
          (define (h/a abt s m)
            (cond [(empty? abt) (max s m)]
                  [else (h/a (node-right abt)
                             (add1 s)
                             (h/a (node-left abt) (add1 s) m))]))]
    (h/a abt0 0 0)))
