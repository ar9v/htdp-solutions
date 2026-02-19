#lang htdp/isl+

;;; Translate this mathematical formula into the ISL+ function `slope`, which maps function
;;; `f` and a number `r1` to the slope of `f` at `r1`. Assume that ε is a global constant.
;;; For your examples, use functions whose exact slope you can figure out, say,
;;; horizontal lines, linear functions, and perhaps polynomials if you know some calculus.

(define ε 0.0001)

; slope: [Number -> Number] Number -> Number
; Computes the slope of the tangent of `f` at `r1`
(check-expect (slope (λ (_x) 3) 5) 0)
(check-expect (slope identity 5) 1)
(check-expect (slope (λ (x) (+ (* 3 x) 1)) 6) 3)
(check-expect (slope (λ (x) (* (- x 2) (- x 4))) 3) 0)
(check-expect (slope (λ (x) (* (- x 2) (- x 4))) 15) 24)
(define (slope f r1)
  (* (/ 1 (* 2 ε)) (- (f (+ r1 ε)) (f (- r1 ε)))))
