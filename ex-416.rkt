#lang htdp/isl+

;;; ISL+ uses `#i0.0` to approximate underflow. Determine the smallest integer `n` such
;;; that `(expt #i10.0 n)` is still an inexact ISL+ number and `(expt #10. (- n 1))` is
;;; approximated with 0.
;;;
;;; HINT: Use a function to compute `n`. Consider abstracting over this function and the
;;; solution of exercise 415.

; underflow-expt: Inexact -> Integer
; Determines the integer for which `(expt ibase (- n 1))` results in #i0.0
(define (underflow-expt ibase)
  (local [(define (go n)
            (if (equal? (expt ibase (- n 1)) #i0.0)
                n
                (go (sub1 n))))]
    (go 0)))

(underflow-expt #i10.0) ; -323

; *flow-expt: Inexact Inexact [N -> N] -> Integer
; Determines the integer for which `(expt ibase (step n))` results in `limit`.
;
; Note: Depending on `step` and `limit`, this may not terminate. E.g.
; -- step = sub1
; -- limit = +inf.0
(define (*flow-expt ibase limit step)
  (local [(define (go n) (if (equal? (expt ibase (step n)) limit) n (go (step n))))]
    (go 0)))

(*flow-expt #i10. +inf.0 add1)
(*flow-expt #i10. #i0.0 sub1)
