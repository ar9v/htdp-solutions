#lang htdp/isl+

;;; Develop the algorithm `integrate-dc`, which integrates a function `f` between the
;;; boundaries `a` and `b` using a divide-and-conquer strategy. Use Kepler's method when
;;; the interval is sufficiently small.

(define ε 0.1)

(define (constant x) 20)
(define (linear x) (* 2 x))
(define (square x) (* 3 (sqr x)))

; integrate-dc: [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume a < b holds
(check-within (integrate-dc constant 12 22) 200 ε)
(check-within (integrate-dc linear 0 10) 100 ε)
(check-within (integrate-dc square 0 10) 1000 ε)
(define (integrate-dc f a b)
  (local [(define mid (/ (+ a b) 2))]
    (cond [(<= (- b a) ε) (* 1/2 (- b a) (+ (f a) (f b)))]
          [else (+ (integrate-dc f a mid) (integrate-dc f mid b))])))
