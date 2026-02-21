#lang htdp/isl+

;;; Design `integrate-adaptive`. That is, turn the recursive process description into an
;;; ISL+ algorithm. Make sure to adapt the test cases from figure 165 to this use.

(define ε 0.1)

(define (constant x) 20)
(define (linear x) (* 2 x))
(define (square x) (* 3 (sqr x)))

; integrate-adaptive: [Number -> Number] Number Number -> Number
; Computes the area under the graph between points `a` and `b`.
;
; assumes a < b
(check-within (integrate-adaptive constant 12 22) 200 ε)
(check-within (integrate-adaptive linear 0 10) 100 ε)
(check-within (integrate-adaptive square 0 10) 1000 ε)
(define (integrate-adaptive f a b)
  (local [(define (trapezoid-area f a b) (* 1/2 (- b a) (+ (f a) (f b))))
          (define mid (/ (+ a b) 2))
          (define area-a (trapezoid-area f a mid))
          (define area-b (trapezoid-area f mid b))]
    (cond [(<= (abs (- area-b area-a)) (* ε (- b a))) (trapezoid-area f a b)]
          [else (+ (integrate-adaptive f a mid) (integrate-adaptive f mid b))])))


;;; Does `integrate-adaptive` always compute a better answer than either `integrate-kepler`
;;; or `integrate-rectangles`? Which aspect is `integrate-adaptive` guaranteed to improve?

;;; A:
;;; 1. Not necessarily! E.g. the regular Kepler method will do equally well for a constant
;;;    function. The other methods will measure up to this method for simpler graphs like
;;;    linear graphs.
;;;
;;; 2. Where this algorithm shines is in performance (w.r.t for the accuracy it affords).
;;;    While it is possible to, e.g. use a really high `R` for `integrate-rectangles` and
;;;    get accurate results, `integrate-rectangles` will definitely take longer to compute
;;;    the area in Figure 166 to get comparable results to `integrate-adaptive`.
