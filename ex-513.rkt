#lang htdp/isl+

;;; Develop a data representation for the same subset of ISL+ that uses structures instead
;;; of lists. Also provide data representations for `ex1`, `ex2`, and `ex3` following your
;;; data definition.

; A Lam is one of
; -- Var
; -- LExpr
; -- App
;
; Var is a Symbol which is not 'λ

(define-struct lexpr [param body])
; An LExpr is a structure
;   (make-lexpr Symbol Lam)
;
; interpretation: (make-lexpr p b) represents a lambda expression with a param `p` and
; body `b`

(define-struct app [fun arg])
; An App (for Application) is a structure
;   (make-app Lam Lam)
;
; interpretation: (make-app f a) represents a function application where `f` is the
; function expression and `a` is the arg expression.

(define ex1 (make-lexpr 'x 'x))
(define ex2 (make-lexpr 'x 'y))
(define ex3 (make-lexpr 'y (make-lexpr 'x 'y)))

(define omega (make-app (make-lexpr 'x (make-app 'x 'x))
                        (make-lexpr 'x (make-app 'x 'x))))
