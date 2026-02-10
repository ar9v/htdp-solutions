#lang htdp/isl+

;;; As this section illustrates, gaps in the data representation lead to round-off errors
;;; when numbers are mapped to Inexes. The problem is, such round-off errors accumulate
;;; across computations.
;;;
;;; Design `add`, a function that adds up `n` copies of #i1/185. For your examples, use
;;; 0 and 1; for the latter, use a tolerance of 0.0001. What is the result for `(add 185)`?
;;; What would you expect? What happens if you multiply the result with a larger number?

(define INEXACT #i1/185)

; add: N -> Number
; Adds up `n` copies of #i1/185
(check-expect (add 0) 0)
(check-within (add 1) INEXACT 0.0001)
; We'd expect this to be 1, but it winds up being ~#i0.9999999999999949
(check-error (check-expect (add 185) 1))
(define (add n)
  (cond [(zero? n) 0]
        [else (+ #i1/185 (add (sub1 n)))]))

;;; Design `sub`. The function counts how often `1/185` can be subtracted from the argument
;;; until it is 0. Use 0 and 1/185 for your examples. What are the expected results? What
;;; are the results for `(sub 1)` and `(sub #i1.0)`? What happend in the second case? Why?

(define RATIONAL 1/185)

; sub: Number -> Number
; Counts how many times `RATIONAL` can be subtracted from `n` until it becomes 0
(check-expect (sub 0) 0)
(check-expect (sub RATIONAL) 1)
(check-expect (sub 1) 185)
(define (sub n)
  (cond [(zero? n) 0]
        [else (+ 1 (sub (- n RATIONAL)))]))

;;; `(sub #i1.0)` will loop forever, since we lose accuracy over the course of
;;; the subtractions, and thus never hit zero(!)
