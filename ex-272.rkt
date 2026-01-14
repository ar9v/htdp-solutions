#lang htdp/isl

(require 2htdp/image)

;;; Recall that the `append` function in ISL concatenates the items of two lists or,
;;; equivalently, replaces '() at the end of the first list with the second list.
;;;
;;; (equal? (append (list 1 2 3) (list 4 5 6 7 8))
;;;         (list 1 2 3 4 5 6 7 8))
;;;
;;; Use `foldr` to define `append-from-fold`. What happens if you replace `foldr` with
;;; `foldl`?
;;;
;;; A: Using `foldl` will flip the order of the first part of the list. `cons` is
;;;    not associative.


; append-from-fold: [List-of X] [List-of X] -> [List-of X]
; `append` but written in terms of `foldr`
(check-expect (append-from-fold (list 1 2 3) (list 4 5 6 7 8))
              (list 1 2 3 4 5 6 7 8))
(define (append-from-fold l1 l2)
  (foldr cons l2 l1))


;;; Now, use one of the fold functions to define functions that compute the sum and the
;;; product, respectively, of a list of numbers.

; sum: [List-of Number] -> [List-of Number]
; Returns the sum of all elements in `l`
(check-expect (sum '()) 0)
(check-expect (sum '(1 2 3)) 6)
(define (sum l)
  (foldr + 0 l))

; product: [List-of Number] -> [List-of Number]
; Returns the product of all elements in `l`
(check-expect (product '()) 1)
(check-expect (product '(1 2 3 4)) 24)
(define (product l)
  (foldr * 1 l))


;;; With one of the fold functions, you can define a function that horizontally composes
;;; a list of Images.
;;;
;;; HINT:
;;; 1. Look up `beside` and `empty-image`. Can you use the other fold function?

; beside-fold: [List-of Image] -> Image
; Renders images in `imgs` horizontally
;
; It is possible to use `foldl`, but the images will be composed in reverse
(define black-square (square 50 "solid" "black"))
(check-expect (beside-fold (list black-square black-square black-square))
              (beside black-square
                      (beside black-square
                              (beside black-square empty-image))))
(define (beside-fold imgs)
  (foldr beside empty-image imgs))

;;; Also, define a function that stacks a list of images vertically.
;;;
;;; HINT:
;;; 1. Check for `above` in the libraries.


; above-fold: [List-of Image] -> Image
; Renders images in `imgs` vertically
;
; Same as with `beside-fold`: Both folds can be used, but `foldl` will stack the images
; provided in reverse.
(check-expect (above-fold (list black-square black-square black-square))
              (above black-square
                      (above black-square
                              (above black-square empty-image))))
(define (above-fold imgs)
  (foldr above empty-image imgs))
