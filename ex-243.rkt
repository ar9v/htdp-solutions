#lang htdp/isl

;;; Assume the definitions area in DrRacket contains

(define (f x) x)

;;; Identify the values among the following expressions:

; (cons f '())                         => (list f)
; (f f)                                => f
; (cons f (cons 10 (cons (f 10) '()))) => (list f 10 10)

;;; Strictly speaking, 10 and '() are values; `cons` and `f` are variables. However, the
;;; shift with respect to BSL is that now, like other variables, they stand for values,
;;; namely their function values! So evaluating `f` by itself yields an error in BSL, but
;;; in ISL its value is the function value of `f`. This is what we'd describe as having
;;; functions that are "first-class". Similarly, these expressions themselves evaluate
;;; to other values, outlined above.
