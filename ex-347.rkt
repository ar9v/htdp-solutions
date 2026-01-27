#lang htdp/isl+

;;; Design `eval-expression`. The function consumes a representation of a BSL expression
;;; and computes its value.

(define-struct add [left right])
(define-struct mul [left right])
; a BSL-expr is one of
; -- Number
; -- (make-add BSL-expr BSL-expr)
; -- (make-mul BSL-expr BSL-expr)
(define ex-1 (make-add 1 1))
(define ex-2 (make-mul 3 10))
(define ex-3 (make-add (make-mul 1 1) 10))

; eval-expression: BSL-Expr -> BSL-Value
; Computes the (BSL) value of `bexpr`
(check-expect (eval-expression 3) 3)
(check-expect (eval-expression ex-1) 2)
(check-expect (eval-expression ex-2) 30)
(check-expect (eval-expression ex-3) 11)
(define (eval-expression bexpr)
  (cond [(number? bexpr) bexpr]
        [(add? bexpr) (+ (eval-expression (add-left bexpr))
                         (eval-expression (add-right bexpr)))]
        [(mul? bexpr) (* (eval-expression (mul-left bexpr))
                         (eval-expression (mul-right bexpr)))]))
