#lang htdp/isl+

(require 2htdp/abstraction)

;;; A linear combination is the sum of many linear terms, that is, products of variables
;;; and numbers. The latter are called coefficients in this context. Here are some
;;; examples:
;;;
;;; -- 5x
;;; -- 5x + 17y
;;; -- 5x + 17y + 3z
;;;
;;; In all examples, the coefficient of `x` is 5, that of `y` is 17, and the one for `z` is
;;; 3.
;;;
;;; If we are given values for variables, we can determine the value of a polynomial. For
;;; example, if x = 10, the value of 5x is 50; if x = 10 and y = 1, the value of
;;; 5x + 17y is 67; and if x = 10, y = 1, and z = 2, the value of 5x + 17y + 3z is 73.
;;;
;;; There are many different representations of linear combinations. We could, for example,
;;; represent them with functions. An alternative representation is a list of its
;;; coefficients. The above combinations would be represented as:
;;;
;;; -- (list 5)
;;; -- (list 5 17)
;;; -- (list 5 17 3)
;;;
;;; This choice of representation assumes a fixed order of variables

;;; Design `value`. The function consumes two equally long lists: a linear combination and
;;; a list of variable values. It produces the value of the combination for these values.

; value: [List-of N] [List-of N] -> N
; Computes the value for linear combination `lc` when given variable values `vals`
;
; Assumption: `lc` and `vals` are the same length.
(check-expect (value '() '()) 0)
(check-expect (value (list 5) (list 10)) 50)
(check-expect (value (list 5 17) (list 10 1)) 67)
(check-expect (value (list 5 17 3) (list 10 1 2)) 73)
(define (value lc vals)
  (cond [(empty? lc) 0]
        [(cons? lc)
         (+ (* (first lc) (first vals))
            (value (rest lc) (rest vals)))]))

;;; Once you have designed that function, you can see that it resembles a fold; but we
;;; need to traverse both lists. You can zip `lc` and `vals`, or use `for/sum`, which
;;; leverages the same idea
(check-expect (value.v2 '() '()) 0)
(check-expect (value.v2 (list 5) (list 10)) 50)
(check-expect (value.v2 (list 5 17) (list 10 1)) 67)
(check-expect (value.v2 (list 5 17 3) (list 10 1 2)) 73)
(define (value.v2 lc vals)
  (for/sum [(co lc) (val vals)] (* co val)))
