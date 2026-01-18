#lang htdp/isl+

;;; Step through the evaluation of the expression

((lambda (x) x) (lambda (x) x))
; returns itself

;;; Now step through this one

((lambda (x) (x x)) (lambda (x) x))
; Same, but with extra steps.

;;; Stop! What do you think we shoul try next?
;;; Yes, try to evaluate

; ((lambda (x) (x x)) (lambda (x) (x x)))

;;; Be ready to hit STOP.

; NOTE: This is known as the omega combinator.
