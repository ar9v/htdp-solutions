#lang htdp/isl+

;;; With a bit of design and a bit of tinkering, a friend of yours came up with the
;;; following solution for the sample problem

(define (relative->absolute l)
  (reverse
   (foldr (λ (f l) (cons (+ f (first l)) l))
          (list (first l))
          (reverse (rest l)))))

;;; The simple solution merely uses well-known ISL+ functions: `reverse` and `foldr`.
;;; Using `lambda`, as you know, is just a convenience. You may also recall from part III
;;; that `foldr` is designable with the design recipes presented in the first two parts
;;; of the book.

;;; Does your friend's solution mean there is no need for our complicated design in this
;;; motivational section?
;;;
;;; A:
;;; It doesn't. `reverse` takes on the order of n^2 steps! So while this is simpler, it
;;; is also more expensive.
