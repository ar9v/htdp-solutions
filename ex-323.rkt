#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `search-bt`. The function consumes a number `n` and a `BT`. If the tree contains
;;; a `node` structure whose `ssn` field is `n`, the function produces the value of the
;;; `name` field in that node. Otherwise, the function produces #false.

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

; search-bt: N BT -> [Maybe Symbol]
; Looks up a symbol by its node number in `bt`. Returns #false if no node has number `n`.
(check-expect (search-bt 15 bt1) 'd)
(check-expect (search-bt 24 bt1) 'i)
(check-expect (search-bt 25 bt1) #false)
(check-expect (search-bt 77 treeA) 'g)
(check-expect (search-bt 24 treeA) 'e)
(define (search-bt n bt)
  (cond [(equal? bt NONE) #false]
        [(= (node-ssn bt) n) (node-name bt)]
        [else (for/or [(maybe-sym (list (search-bt n (node-left bt))
                                        (search-bt n (node-right bt))))]
                maybe-sym)]))
