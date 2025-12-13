#lang htdp/bsl

;;; Design the function `add-to-pi`. It consumes a natural number `n` and adds it to `pi`
;;; *without* using the primitive + operation. Here is a start:

; add-to-pi: N -> Number
; computes (+ n pi) without using +
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
(define (add-to-pi n)
  (cond [(zero? n) pi]
        [(positive? n) (add1 (add-to-pi (sub1 n)))]))


;;; Once you have a complete definition, generalize the function to `add`, which adds
;;; a natural number `n` to some arbitrary number `x` without using `+`. Why does the
;;; skeleton use `check-within`?

; add: N Number -> Number
; computes (+ n x) without using +
(check-within (add 0 2) (+ 0 2) 0.001)
(check-within (add 1 2) (+ 1 2) 0.001)
(check-within (add 3 4.7) (+ 3 4.7) 0.001)
(define (add n x)
  (cond [(zero? n) x]
        [(positive? n) (add1 (add (sub1 n) x))]))


;;; A: We use check within because in `add-to-pi`, pi is irrational, and in `add`, the
;;;    other parameter is an arbitrary number (i.e. not necessarily an Integer or a
;;;    Natural)
