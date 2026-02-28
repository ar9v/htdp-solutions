#lang htdp/isl+

;;; Design an accumulator-style version of `product`, the function that computes the
;;; product of a list of numbers. Stop when you have formulated the accumulator invariant
;;; and have someone check it.

; product: [List-of N] -> N
; Computes the result of multiplying all numbers in `l`
(check-expect (product '(1 4 3)) 12)
(check-expect (product '(-2 -4 -5)) -40)
(define (product l)
  (local [; p/a: [List-of N] N -> N
          ; Computes the result of multiplying all numbers in `sub-l`.
          ; accumulator a: the product of all numbers in `l` not in `sub-l`.
          (define (p/a sub-l a)
            (cond [(empty? sub-l) a]
                  [else (p/a (rest sub-l) (* (first sub-l) a))]))]
    (p/a l 1)))

;;; The performance of `product` is `O(n)` where `n` is the length of the list. Does the
;;; accumulator version improve on this?
;;;
;;; A:
;;; No, at least not time-wise; we still have to go through all elements to compute their
;;; product.
