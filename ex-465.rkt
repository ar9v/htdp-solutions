#lang htdp/isl+

;;; Design `subtract`. The function consumes two Equations of equal length. It "subtracts"
;;; a multiple of the second equation from the first, item by item, so that the resulting
;;; Equation has a 0 in the first position. Since the leading coefficient is known to be
;;; 0, `subtract` returns the rest of the list that results from the subtractions.

; subtract: Equation Equation -> Equation
; Subtracts a multiple of `e2` from `e1`, item by item, so as to produce a 0 in the first
; position; returns the non-zero coefficients of the result.
(check-expect (subtract '(3 4) '(1 2)) '(-2))
(check-expect (subtract '(8 3 20) '(2 4 16)) '(-13 -44))
(check-expect (subtract '(-20 4 -18) '(4 2 -3)) '(14 -33))

; This test illustrates that we face similar tradeoffs/pitfalls with this function as we
; do when finding roots with and without exact numbers: if the coefficients are rationals,
; we can always find the exact multiple by dividing the minuend by the subtrahend;
; if they're not, we'll wind up with an inexact.
(check-expect (subtract '(7 2 10) '(2 3 8)) '(-17/2 -18))
(define (subtract e1 e2)
  (local [(define k (/ (first e1) (first e2)))]
    (rest (map - e1 (map (scale k) e2)))))

; scale: Number -> [Number -> Number]
; Returns a function that will multiply a given number by `n`
(define (scale n) (λ (x) (* n x)))
