#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `subst`. The function consumes a BSL-var-expr `ex`, a Symbol `x`, and a Number
;;; `v`. It produces a BSL-var-expr like `ex` with all occurrences of `x` replaced by `v`.

(define-struct add [left right])
(define-struct mul [left right])
; a BSL-var-exp is one of:
; -- Number
; -- Symbol
; -- (make-add BSL-var-expr BSL-var-expr)
; -- (make-mul BSL-var-expr BSL-var-expr)
(define bexpr-ex (make-add 'x 'y))

; subst: BSL-var-expr Symbol Number -> BSL-var-expr
; Replaces all occurrences of `x` in `bexpr` with `v`
(check-expect (subst 3 'z 1) 3)
(check-expect (subst 'x 'x 4) 4)
(check-expect (subst bexpr-ex 'x 3) (make-add 3 'y))
(check-expect (subst bexpr-ex 'y 3) (make-add 'x 3))
(check-expect (subst (make-add (make-mul 3 'x) (make-add 4 'x)) 'x 8)
              (make-add (make-mul 3 8) (make-add 4 8)))
(define (subst bexpr s v)
  (match bexpr
    [(? number?) bexpr]
    [(? symbol?) (if (equal? bexpr s) v bexpr)]
    [(add l r) (make-add (subst l s v) (subst r s v))]
    [(mul l r) (make-mul (subst l s v) (subst r s v))]))
