#lang htdp/bsl

;;; Design the functions `set+.L` and `set+.R`, which create a set by adding a number
;;; `x` to some given set `s` fro the left-hand and right-hand data definition,
;;; respectively

; A Son.L is one of:
; -- empty
; -- (cons Number Son.L)
;
; Son is used when it
; applies to Son.L and Son.R

; A Son.R is one of:
; -- empty
; -- (cons Number Son.R)
;
; Constraint If s is a Son.R,
; no number occurs twice in s

; set+.L: Number Son.L -> Son.L
; Adds `x` to `son`
(check-expect (set+.L 1 '()) (cons 1 '()))
(check-expect (set+.L 1 (cons 1 '())) (cons 1 (cons 1 '())))
(check-expect (set+.L 2 (cons 1 '())) (cons 2 (cons 1 '())))
(define (set+.L x son)
  (cons x son))

; set+.R: Number Son.R -> Son.R
; Adds `x` to `son`
(check-expect (set+.R 1 '()) (cons 1 '()))
(check-expect (set+.R 1 (cons 1 '())) (cons 1 '()))
(check-expect (set+.R 2 (cons 1 '())) (cons 2 (cons 1 '())))
(define (set+.R x son)
  (if (member? x son) son (cons x son)))
