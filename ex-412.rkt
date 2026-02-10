#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `inex+`. The function adds two Inex representations of numbers that have the
;;; same exponent. The function must be able to deal with inputs that increase the
;;; exponent. Furthermore, it must signal its own error if the result is out of range, not
;;; rely on `create-inex` for error checking.

(define-struct inex [mantissa sign exponent])

; create-inex: N Number N -> Inex
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
; -- inex1 and inex2 have the exponents with a difference of at most 1
(check-expect (inex+ (create-inex 1 1 0) (create-inex 2 1 0))
              (create-inex 3 1 0))
(check-expect (inex+ (create-inex 3 1 2) (create-inex 99 1 2))
              (create-inex (round (/ (+ 99 3) 10)) 1 3))
(check-expect (inex+ (create-inex 99 -1 99) (create-inex 3 -1 99))
              (create-inex (round (/ (+ 99 3) 10)) -1 98))
(check-error (inex+ MAX-POSITIVE (create-inex 1 1 99)))
(define (inex+ inex1 inex2)
  (match (list inex1 inex2)
    [(list (inex m1 s1 e1) (inex m2 s2 e2))
     (local [(define (normalize-inex i)
               (match i
                 [(inex m s e)
                  (cond [(<= m 99) i]
                        [else (make-inex (round (/ m 10)) s (abs (add1 (* s e))))])]))
             (define normalized-inex
               (normalize-inex
                (cond [(< (* s1 e1) (* s2 e2)) (make-inex (+ m1 (* 10 m2)) s1 e1)]
                      [(= (* s1 e1) (* s2 e2)) (make-inex (+ m1 m2) s1 e1)]
                      [else (make-inex (+ (* 10 m1) m2) s2 e2)])))]
       (if (<= (inex-exponent normalized-inex) 99)
           normalized-inex
           (error "Error: inexact number addition overflowed!")))]))

;;; Challenge:
;;; Extend `inex+` so that it can deal with inputs whose exponents differ by 1
(check-expect
 (inex+ (create-inex 1 1 0) (create-inex 1 -1 1))
 (create-inex 11 -1 1))

(check-expect
 (inex+ (create-inex 2 -1 2) (create-inex 4 -1 1))
 (create-inex 42 -1 2))

(check-expect
 (inex+ (create-inex 4 1 2) (create-inex 5 1 1))
 (create-inex 45 1 1))
