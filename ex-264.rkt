#lang htdp/isl

;;; Use DrRacket's stepper to calculate out how it evaluates `(sup (list 2 1 3))`, where
;;; `sup` is the function from figure 89 equipped with `local`.

; Nelon -> Number
; determines the largest number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (local ((define largest-in-rest (sup (rest l))))
       (if (> (first l) largest-in-rest)
           (first l)
           largest-in-rest))]))

;;; I'm adding the function here for convenience, but as stated in 263.rkt, this is way
;;; easier to grok from the stepper, since `local` will produce two expressions when it
;;; is evaluated.
