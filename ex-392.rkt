#lang htdp/isl+

;;; Simplify the function `tree-pick` from exercise 390.

;;; I'll again do this for both interpretations

(define-struct branch [left right])

; tree-pick: TOS [List-of Direction] -> [Maybe TOS]
; Returns the sub-TOS in `tos` that you'd arrive at by following the list of directions
; `lod`, if any at all; signals an error otherwise.
;
; NOTE: I had already simplified this in the commit for 390.
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

; tree-pick-sym: TOS [List-of Direction] -> [Maybe TOS]
; Returns the symbol in `tos` that you'd arrive at by following the list of directions
; `lod`, if any at all; signals an error otherwise.
;
; NOTE: We can simplify this if we have a generic error for both erroring cases,
; but we can't really do anything else.
(check-expect (tree-pick-sym 'foo '()) 'foo)
(check-error (tree-pick-sym 'foo '(left)))
(check-expect (tree-pick-sym (make-branch 'a 'b) '(left)) 'a)
(check-error (tree-pick-sym (make-branch (make-branch 'a 'b) 'c) '(left)))
(check-error (tree-pick-sym (make-branch 'a b) '(left right)))
(define (tree-pick-sym tos lod)
  (cond [(and (symbol? tos) (empty? lod)) tos]
        [(or (and (symbol? tos) (cons? lod)) (and (branch? tos) (empty? lod)))
         (error "Error: Reached a symbol with directions remaining or ran out of nodes!")]
        [(and (branch? tos) (cons? lod))
         (tree-pick-sym
          ((if (equal? (first lod) 'left) branch-left branch-right) tos)
          (rest lod))]))
