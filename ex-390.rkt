#lang htdp/isl+

;;; Design the function `tree-pick`. The function consumes a tree of symbols and a list of
;;; directions:

(define-struct branch [left right])
; A TOS is one of:
; -- Symbol
; -- (make-branch TOS TOS)

; A Direction is one of
; -- 'left
; -- 'right

; A list of `Direction`s is also called a path

;;; Clearly a Direction tells the function whether to choose the left or right branch in
;;; a nonsymbolic tree. What is the result of the `tree-pick` function? Don't forget to
;;; formulate a full signature. The function signals an error when given a symbol and
;;; a non-empty path.

; tree-pick: TOS [List-of Direction] -> [Maybe TOS]
; Returns the sub-TOS in `tos` that you'd arrive at by following the list of directions
; `lod`, if any at all; signals an error otherwise.
(check-expect (tree-pick 'foo '()) 'foo)
(check-error (tree-pick 'foo '(left)))
(check-expect (tree-pick (make-branch 'a 'b) '(left)) 'a)
(check-expect (tree-pick (make-branch (make-branch 'a 'b) 'c) '(left)) (make-branch 'a 'b))
(check-error (tree-pick (make-branch 'a b) '(left right)))
(define (tree-pick tos lod)
  (cond [(empty? lod) tos]
        [(symbol? tos) (error "Reached the end of the tree with directions remaining!")]
        [(branch? tos)
         (tree-pick ((if (equal? (first lod) 'left) branch-left branch-right) tos)
                    (rest lod))]))

;;; The problem statement could also be interpreted to require that a list of directions
;;; necessarily point to a leaf (i.e. a symbol). The resulting program wouldn't be the same
;;; but the same ideas would apply, viz. we'd start out with 4 `cond` branches, one for each
;;; case.

; tree-pick-sym: TOS [List-of Direction] -> [Maybe TOS]
; Returns the symbol in `tos` that you'd arrive at by following the list of directions
; `lod`, if any at all; signals an error otherwise.
(check-expect (tree-pick-sym 'foo '()) 'foo)
(check-error (tree-pick-sym 'foo '(left)))
(check-expect (tree-pick-sym (make-branch 'a 'b) '(left)) 'a)
(check-error (tree-pick-sym (make-branch (make-branch 'a 'b) 'c) '(left)))
(check-error (tree-pick-sym (make-branch 'a b) '(left right)))
(define (tree-pick-sym tos lod)
  (cond [(and (symbol? tos) (empty? lod)) tos]
        [(and (symbol? tos) (cons? lod))
         (error "Reached the end of the tree with directions remaining!")]
        [(and (branch? tos) (empty? lod))
         (error "Ran out of directions, but we haven't reached a symbol yet!")]
        [(and (branch? tos) (cons? lod))
         (tree-pick-sym
          ((if (equal? (first lod) 'left) branch-left branch-right) tos)
          (rest lod))]))
