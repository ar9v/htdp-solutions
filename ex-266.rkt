#lang htdp/isl

;;; Use DrRacket's stepper to find out how ISL evaluates
;;;
;;; ((local ((define (f x) (+ x 3))
;;;          (define (g x) (* x 4)))
;;;    (if (odd? (f (g 1)))
;;;        f
;;;        g))
;;;  2)
