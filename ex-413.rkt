#lang htdp/isl+

;;; Design `inex*`. The function multiplies two Inex representations of numbers, including
;;; inputs that force an additional increase of the output's exponent. Like `inex+`, it
;;; must signal its own error if the result is out of range, not rely on `create-inex` to
;;; perform error checking.

(define-struct inex [mantissa sign exponent])

; create-inex: N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))

; inex*: Inex Inex -> Inex
; Multiplies two Inex representations
(check-expect (inex* (create-inex 2 1 10) (create-inex 7 1 3))
              (create-inex 14 1 13))
(check-expect (inex* (create-inex 10 1 30) (create-inex 20 1 20))
              (create-inex 20 1 51))
(check-expect (inex* (create-inex 10 1 30) (create-inex 20 -1 20))
              (create-inex 20 1 11))
(check-expect (inex* (create-inex 10 1 20) (create-inex 20 -1 30))
              (create-inex 20 -1 9))
(check-error (inex* (create-inex 1 -1 99) (create-inex 1 -1 2)))
(check-error (inex* (create-inex 2 1 99) (create-inex 1 1 2)))
(define (inex* i1 i2)
  (local [(define (normalize i)
            (local [(define m (inex-mantissa i))
                    (define s (inex-sign i))
                    (define e (inex-exponent i))]
              (if (< m 99)
                  i
                  (normalize (make-inex (round (/ m 10)) s (+ s (* s s e)))))))
          (define m1 (inex-mantissa i1))
          (define m2 (inex-mantissa i2))
          (define s1 (inex-sign i1))
          (define s2 (inex-sign i2))
          (define e1 (inex-exponent i1))
          (define e2 (inex-exponent i2))
          (define signed-exp-sum (+ (* s1 e1) (* s2 e2)))
          (define res-sign (if (positive? signed-exp-sum) 1 -1))
          (define normalized
            (normalize (make-inex (* m1 m2) res-sign (abs signed-exp-sum))))]
    (cond [(inex< normalized MIN-POSITIVE) (error "Error: the multiplication underflowed!")]
          [(inex> normalized MAX-POSITIVE) (error "Error: the multiplication overflowed!")]
          [else normalized])))

; inex->number: Inex -> Number
; converts an inex into its numeric equivalent
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt
      10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

; inex<: Inex Inex -> Boolean
; Does i1 represent a smaller number than i2?
(define (inex< i1 i2) (< (inex->number i1) (inex->number i2)))

; inex>: Inex Inex -> Boolean
; Does i1 represent a larger number than i2?
(define (inex> i1 i2) (> (inex->number i1) (inex->number i2)))
