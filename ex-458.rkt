#lang htdp/isl+

;;; Kepler suggested asimple integration method. To compute an estimate of the area under
;;; `f` between `a` and `b`, proceed as follows:
;;;
;;; 1. Divide the interval into half at mid = (a + b) / 2
;;; 2. Compute the areas of these two trapezoids:
;;;    - [(a, 0), (a, f(a)), (mid, 0), (mid, f(mid))]
;;;    - [(mid, 0), (mid, f(mid)), (b, 0), (b, f(b))]
;;; 3. Then add the two areas
;;;
;;; The area of a trapezoid is given by 1/2 * (R - L) * [f(L) + f(R)]

;;; Design the function `integrate-kepler`. That is, turn the mathematical knowledge
;;; into an ISL+ function. Adapt the test cases from figure 165 to this use. Which of
;;; the three tests fails and by how much?

(define ε 0.1)

(define (constant x) 20)
(define (linear x) (* 2 x))
(define (square x) (* 3 (sqr x)))

; integrate-kepler: [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume a < b holds
(check-within (integrate-kepler constant 12 22) 200 ε)
(check-within (integrate-kepler linear 0 10) 100 ε)
; (check-within (integrate-kepler square 0 10) 1000 ε) ; 1125, fails by 125 (or 12.5%)
(define (integrate-kepler f a b)
  (local [(define mid (/ (+ a b) 2))
          (define (trapezoid-area f a b) (* 1/2 (- b a) (+ (f a) (f b))))]
    (+ (trapezoid-area f a mid) (trapezoid-area f mid b))))
