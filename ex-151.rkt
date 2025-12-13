#lang htdp/bsl

;;; Design the function `multiply`. It consumes a natural number `n` and multiplies it
;;; with a number `x` without using `*`.

; multiply: N Number -> Number
; Computes `(* n x)` without using *
(check-expect (multiply 0 3) 0)
(check-expect (multiply 1 3) 3)
(check-within (multiply 2 3) (* 2 3) 0.001)
(check-within (multiply 4 8.2) (* 4 8.2) 0.001)
(define (multiply n x)
  (cond [(zero? n) 0]
        [(positive? n) (+ x (multiply (sub1 n) x))]))

;;; Use DrRacket's stepper to evaluate `(multiply 3 x) for any `x` you like. How does
;;; `multiply` relate to what you know from grade school?

;;; A: In grade school we're taught that multiplying is repeatedly adding a number a given
;;;    number of times. Here, `multiply` is defined the same way: we add `x`, `n` times.
