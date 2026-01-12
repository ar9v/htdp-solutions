#lang htdp/isl

;;; Each of these signatures describes a class of functions. Describe these collections
;;; with at least one example from ISL.

; [Number -> Boolean]
; positive?, zero?, negative?

; [Boolean String -> Boolean]
(define (boolean-string=? b s)
  (or (and (false? b) (string=? s "#false"))
      (and b (string=? s "#true"))))

; [Number Number Number -> Number]
(define (between? a x b)
  (and (< a x) (< x b)))

; [Number -> [List-of Number]]
(define (reverse-iota n)
  (cond [(zero? n) '()]
        [(positive? n) (cons n (reverse-iota (sub1 n)))]))

; [[List-of Number] -> Boolean]
(define (all-positive? ns)
  (andmap positive? ns))
