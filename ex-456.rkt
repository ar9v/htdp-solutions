#lang htdp/isl+

;;; Design `root-of-tangent`, a function that maps `f` and `r1` to the root of the tangent
;;; through (r1, (f r1)).

(define ε 0.0001)

; root-of-tangent: [Number -> Number] Number -> Number
; Given `f` and `r1`, computes the root of the tangent
(check-error (root-of-tangent (λ (_x) 3) 5))
(check-expect (root-of-tangent identity 5) 0)
(check-expect (root-of-tangent (λ (x) (+ (* 3 x) 1)) 6) (- 6 19/3))
(check-error (root-of-tangent (λ (x) (* (- x 2) (- x 4))) 3))
(check-expect (root-of-tangent (λ (x) (* (- x 2) (- x 4))) 15) (- 15 143/24))
(define (root-of-tangent f r1)
  (- r1 (/ (f r1) (slope f r1))))

; slope: [Number -> Number] Number -> Number
; Computes the slope of the tangent of `f` at `r1`
(check-expect (slope (λ (_x) 3) 5) 0)
(check-expect (slope identity 5) 1)
(check-expect (slope (λ (x) (+ (* 3 x) 1)) 6) 3)
(check-expect (slope (λ (x) (* (- x 2) (- x 4))) 3) 0)
(check-expect (slope (λ (x) (* (- x 2) (- x 4))) 15) 24)
(define (slope f r1)
  (* (/ 1 (* 2 ε)) (- (f (+ r1 ε)) (f (- r1 ε)))))
