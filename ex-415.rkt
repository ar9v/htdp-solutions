#lang htdp/isl+

;;; ISL+ uses `+inf.0` to deal with overflow. Deteermine the integer `n` such that
;;;
;;; `(expt #i10.0 n)`
;;;
;;; is an inexact number while `(expt #i10. (+ n 1))` is approximated with `+inf.0`
;;;
;;; HINT: Design a function to compute `n`.

; overflow-expt: Inexact -> Integer
; Given an inexact `ibase`, returns the integer for which `(expt ibase (+ n 1))`
; is approximated with `+inf.0`
(define (overflow-expt ibase)
  (local [(define (go n) (if (equal? (expt ibase (+ n 1)) +inf.0) n (go (+ n 1))))]
    (go 0)))

(overflow-expt #i10.0) ; 308
