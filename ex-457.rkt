#lang htdp/isl+

;;; Design the function `double-amount`, which computes how many months it takes to double
;;; a given amount of money when a savings account pays interest at a fixed rate on a
;;; monthly basis
;;;
;;; Domain Knowledge: With a minor algebraic manipulation, you can show that the given
;;; amount is irrelevant. Only the interest rate matters. Also domain experts know that
;;; doubling occurs after roughly 72/r month as long as the interest r is "small".

; double-amount: Number Number -> Number
; Computes the amount of months it takes to double `amount` given `rate`, where
; `amount` represents cents and `r` is a percentage (e.g. 7 is a 7% interest rate)
;
; generative: It computes the amount we'd have by the next period, and compares it to
; the original amount's double.
;
; termination: Requires a positive rate; a non-positive rate will never increment an amount
; and therefore will cause `double-amount` to loop forever.
;
; Note: We check-within one because we move in 1-month increments; testing vs the rule of
; 72 loses precision outside of a range between 4 and 20 percent.
(check-within (double-amount 100 7) (/ 72 7) 1)
(check-within (double-amount 2000 7) (/ 72 7) 1)
(check-within (double-amount 125 4) (/ 72 4) 1)
(define (double-amount amount r)
  (local [(define double (* amount 2))
          ; per the book, (without this) try:
          ;   (double-amount 123 (/ 4 99)) vs
          ;   (double-amount 123 (exact->inexact (/ 4 99)))
          ; and have fun
          (define rate (exact->inexact (/ r 100)))
          (define (next a) (+ a (* a rate)))
          (define (double-amount-helper a months)
            (cond [(<= double a) months]
                  [else (double-amount-helper (next a) (add1 months))]))]
    (double-amount-helper amount 0)))
