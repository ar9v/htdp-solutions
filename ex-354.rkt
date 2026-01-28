#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `eval-variable`. The checked function consumes a `BSL-var-expr` and determines
;;; its value if `numeric?` yields true for the input. Otherwise it signals an error.

(define-struct add [left right])
(define-struct mul [left right])
; a BSL-expr is one of
; -- Number
; -- (make-add BSL-var-expr BSL-var-expr)
; -- (make-mul BSL-var-expr BSL-var-expr)
;
; a BSL-var-exp is one of:
; -- Number
; -- Symbol
; -- (make-add BSL-var-expr BSL-var-expr)
; -- (make-mul BSL-var-expr BSL-var-expr)
(define bvexpr1 (make-add 'x 'y))
(define bvexpr2 (make-add (make-add (make-add 1 1) 3) (make-mul 2 3)))
(define bvexpr3 (make-add (make-add (make-add 1 1) 'x) (make-mul 2 3)))
(define bvexpr4 (make-mul 'y 'a))

; eval-variable: BSL-var-expr -> [Maybe BSL-value]
(check-expect (eval-variable 1) 1)
(check-error (eval-variable 's))
(check-error (eval-variable bvexpr1))
(check-expect (eval-variable bvexpr2) 11)
(check-error (eval-variable bvexpr3))
(define (eval-variable bvexpr)
  (if (numeric? bvexpr)
      (eval-expression bvexpr)
      (error "Error: undefined variables in expression")))

; numeric?: BSL-var-expr -> Boolean
; Determines whether BSL-var-expr `bvexpr` is also a BSL-expr, i.e. if it has no symbols
; in it
(define (numeric? bvexpr)
  (match bvexpr
    [(? number?) #true]
    [(? symbol?) #false]
    [(add l r) (and (numeric? l) (numeric? r))]
    [(mul l r) (and (numeric? l) (numeric? r))]))

; eval-expression: BSL-Expr -> BSL-Value
; Computes the (BSL) value of `bexpr`
(define (eval-expression bexpr)
  (match bexpr
    [(? number?) bexpr]
    [(add l r) (+ (eval-expression l) (eval-expression r))]
    [(mul l r) (* (eval-expression l) (eval-expression r))]))


;;; In general, a program defines many constants in the definitions area, and expressions
;;; contain more than one variable. To evaluate such expressions, we need a representation
;;; of the definitions area when it contains a series of constants definitions. For
;;; this exercise we use association lists. Make up elements of AL

; An AL (short for Association List) is [List-of Association]
; An Association is a list of two items:
;   (cons Symbol (cons Number '()))
(define al1 '((x 3) (y 4) (z 1)))
(define al2 '((y 4) (a 9)))

;;; Design `eval-variable*`. The function consumes a BSL-var-expr `ex` and an association
;;; list `da`. Starting from `ex`, it iteratively applies `subst` to all associations in
;;; `da`. If `numeric?` holds for the result, it determines its value; otherwise it signals
;;; the same error as `eval-variable`.

; eval-variable*: BSL-var-expr AL -> [Maybe BSL-val]
; Uses AL `da` to provide values for symbols in `bvexpr`, and tries to evaluate it.
(check-expect (eval-variable* bvexpr2 '()) (eval-variable bvexpr2))
(check-error (eval-variable* bvexpr1 '((x 3))))
(check-expect (eval-variable* bvexpr3 '((x 1))) 9)
(check-expect (eval-variable* bvexpr1 al1) 7)
(check-expect (eval-variable* bvexpr4 al2) 36)
(define (eval-variable* bvexpr da)
  (local [(define subst-bvexpr
            (foldr (λ (assc res) (subst res (first assc) (second assc))) bvexpr da))]
    (eval-variable subst-bvexpr)))

; subst: BSL-var-expr Symbol Number -> BSL-var-expr
; Replaces all occurrences of `x` in `bexpr` with `v`
(define (subst bexpr s v)
  (match bexpr
    [(? number?) bexpr]
    [(? symbol?) (if (equal? bexpr s) v bexpr)]
    [(add l r) (make-add (subst l s v) (subst r s v))]
    [(mul l r) (make-mul (subst l s v) (subst r s v))]))
