#lang htdp/isl+

;;; Design `inex+`. The function adds two Inex representations of numbers that have the
;;; same exponent. The function must be able to deal with inputs that increase the
;;; exponent. Furthermore, it must signal its own error if the result is out of range, not
;;; rely on `create-inex` for error checking.

(define-struct inex [mantissa sign exponent])
; An Inex is a structure:
;   (make-inex N99 S N99)
;
; An S is one of:
; -- -1
; -- 1
;
; An N99 is an N between 0 and 99 (inclusive)

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))

(define MAX-POSITIVE (create-inex 99 1 99))

; inex+: Inex Inex -> Inex
; Adds two Inex representations.
;
; Assumptions:
; -- inex1 and inex2 have the same exponent.
(check-expect (inex+ (create-inex 1 1 0) (create-inex 2 1 0))
              (create-inex 3 1 0))
(check-expect (inex+ (create-inex 3 1 2) (create-inex 99 1 2))
              (create-inex (round (/ (+ 99 3) 10)) 1 3))
(check-expect (inex+ (create-inex 99 -1 99) (create-inex 3 -1 99))
              (create-inex (round (/ (+ 99 3) 10)) -1 98))
(check-error (inex+ MAX-POSITIVE (create-inex 1 1 99)))
(define (inex+ inex1 inex2)
  (local [(define (normalize-inex i)
            (local [(define mantissa (inex-mantissa i))]
              (cond [(<= mantissa 99) i]
                    [else (make-inex (round (/ mantissa 10))
                                     (inex-sign i)
                                     (* sign (add1 (* sign (inex-exponent i)))))])))
          (define mantissa (+ (inex-mantissa inex1) (inex-mantissa inex2)))
          (define sign (inex-sign inex1))
          (define exponent (inex-exponent inex1))
          (define normalized-inex (normalize-inex (make-inex mantissa sign exponent)))]
    (if (<= (inex-exponent normalized-inex) 99)
        normalized-inex
        (error "Error: inexact number addition overflowed!"))))

;;; Challenge:
;;; Extend `inex+` so that it can deal with inputs whose exponents differ by 1
(check-expect
 (inex+ (create-inex 1 1 0) (create-inex 1 -1 1))
 (create-inex 11 -1 1))
