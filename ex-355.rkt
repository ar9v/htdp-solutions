#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `eval-var-lookup`. This function has the same signature as `eval-variable*`:

; eval-var-lookup: BSL-var-expr AL -> BSL-value

;;; Intead of using substitution, the function traverses the expression in the manner that
;;; the design recipe for BSL-var-expr suggests. As it descends the expression, it "carries
;;; along" `da`. When it encounters a symbol `x`, it uses `assq` to look up the value of
;;; `x` in the association list. If there is no value, `eval-var-lookup` signals an error.

(define-struct add [left right])
(define-struct mul [left right])
; a BSL-var-exp is one of:
; -- Number
; -- Symbol
; -- (make-add BSL-var-expr BSL-var-expr)
; -- (make-mul BSL-var-expr BSL-var-expr)
(define bvexpr1 (make-add 'x 'y))
(define bvexpr2 (make-add (make-add (make-add 1 1) 'x) (make-mul 2 3)))
(define bvexpr3 (make-mul 'y 'a))

(define al1 '((x 3) (y 1)))
(define al2 '((x 2) (a 8) (y 4)))

; eval-var-lookup: BSL-var-expr AL -> BSL-value
; Evaluates `bvexpr` using the environment model, looking up definitions in `da` as needed.
; Evaluation fails if the function encounters a variable that is not found in `da`
(check-expect (eval-var-lookup 1 al1) 1)
(check-expect (eval-var-lookup 'x al1) 3)
(check-error (eval-var-lookup 'z al1))
(check-expect (eval-var-lookup bvexpr1 al1) 4)
(check-expect (eval-var-lookup bvexpr2 al2) 10)
(check-error (eval-var-lookup bvexpr3 al1))
(check-expect (eval-var-lookup bvexpr3 al2) 32)
(define (eval-var-lookup bvexpr da)
  (match bvexpr
    [(? number?) bvexpr]
    [(? symbol?)
     (match (assq bvexpr da)
       [(list _sym val) val]
       [_false (error "Error: " bvexpr " is unbound!")])]
    [(add l r) (+ (eval-var-lookup l da) (eval-var-lookup r da))]
    [(mul l r) (* (eval-var-lookup l da) (eval-var-lookup r da))]))
