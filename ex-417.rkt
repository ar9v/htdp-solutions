#lang htdp/isl+

;;; Evaluate `(expt 1.001 1e-12)` in Racket and ISL+. Explain what you see

(expt 1.001 1e-12)

; Racket: 1.000000000000001
; ISL:  #i1.000000000000001

;;; Per the book, Racket interprets all decimal numbers as inexact, and renders all reals
;;; as decimals; hence the lack of a prefix.
