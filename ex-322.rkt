#lang htdp/isl+

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

(define bt2
  (make-node
   15
   'd
   (make-node 87 'h NONE NONE)
   NONE))

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

;;; Draw the above two trees in the manner of figure 119

; 15 --
;     |
;     24

;  -- 15
; |
; 84

;;; Design `contains-bt?`, which determines whether a given number occurs in some given
;;; BT

; contains-bt?: BT Number -> Boolean
; Determines if `n` is an `ssn` in `bt`
(check-expect (contains-bt? bt1 30) #false)
(check-expect (contains-bt? bt1 15) #true)
(check-expect (contains-bt? bt1 24) #true)
(check-expect (contains-bt? bt2 15) #true)
(check-expect (contains-bt? bt2 87) #true)
(check-expect (contains-bt? bt2 24) #false)
(check-expect (contains-bt? treeA 99) #true)
(define (contains-bt? bt n)
  (cond [(equal? bt NONE) #false]
        [else (or (= (node-ssn bt) n)
                  (contains-bt? (node-left bt) n)
                  (contains-bt? (node-right bt) n))]))
