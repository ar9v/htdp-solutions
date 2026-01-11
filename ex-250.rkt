#lang htdp/isl

;;; Design `tabulate`, which is the abstraction of the two functions in figure 92. When
;;; `tabulate` is properly designed, use it to define a tabulation function for `sqr` and
;;; `tan`.

; tabulate: Number [Number -> Number] -> [List-of Number]
; Tabulates `f` from `n` to 0, inclusive
;
; constraint: `n` is positive
(check-expect (tabulate 3 identity) (list 3 2 1 0))
(check-expect (tabulate 3 sqr) (list 9 4 1 0))
(define (tabulate n f)
  (cond [(zero? n) (list (f n))]
        [(positive? n) (cons (f n) (tabulate (sub1 n) f))]))

; tabulate-sqr: Number -> [List-of Number]
; Tabulates the `sqr` function, from n to 0 (inclusive)
(check-expect (tabulate-sqr 5) (list 25 16 9 4 1 0))
(define (tabulate-sqr n)
  (tabulate n sqr))

; tabulate-tan: Number -> [List-of Number]
; Tabulates the `tan` function, from n to 0 (inclusive)
(check-within (tabulate-tan 5)
              (list (tan 5) (tan 4) (tan 3) (tan 2) (tan 1) (tan 0))
              0.1)
(define (tabulate-tan n)
  (tabulate n tan))
