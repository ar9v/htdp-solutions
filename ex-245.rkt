#lang htdp/isl

;;; Develop the `function=at-1.2-3-and-5.775?` function. Given two functions from
;;; numbers to numbers, the function determines whether the two produce the same results
;;; for 1.2, 3, and -5.775.
;;;
;;; Mathematicians say that two functions are equal if they compute the same result when
;;; given the same input -- for all possible inputs.

; function=at-1.2-3-and-5.775: (Number -> Number) (Number -> Number) -> Boolean
; Determines whether `f` and `g` produce the same results for
;
; 1.2, 3, and -5.775
(check-expect (function=at-1.2-3-and-5.775 identity identity) #true)
(define (function=at-1.2-3-and-5.775 f g)
  (and (= (f 1.2) (g 1.2))
       (= (f 3) (g 3))
       (= (f -5.755) (g -5.755))))

;;; Can we hope to define `function=?`, which determines whether two functions from
;;; numbers to numbers are equal? If so, define the function. If not, explain why and
;;; consider the implication that you have encountered the first easily definable idea
;;; for which you cannot define a function.
;;;
;;; A:
;;; We cannot! That'd imply we're able to verify that the functions return the same result
;;; for _every_ number, but the set of all numbers is infinite!
