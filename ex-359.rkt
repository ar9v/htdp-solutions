#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `eval-function*`. The function consumes `ex`, a BSL-fun-expr, and `da`, a
;;; BSL-fun-def* representation of a definitions area. It produces the result that
;;; DrRacket shows if you evaluate `ex` in the interactions area, assuming the definitions
;;; area contains `da`.
;;;
;;; The function works like `eval-definition1` from exercise 357. For an application of
;;; some function `f`, it
;;;
;;; 1. Evaluates the argument
;;; 2. Looks up the definition of `f` in the BSL-fun-def representation of `da`, which
;;;    comes with a parameter and a body;
;;; 3. Substitutes the value of the argument for the function parameter in the function's
;;;    body; and
;;; 4. Evaluates the new expression via recursion
;;;
;;; Like DrRacket, `eval-function*` signals an error when it encounters a variable or
;;; function name without definition in the definitions area.

(define-struct add [left right])
(define-struct mul [left right])
(define-struct funcall [name arg])
; a BSL-fun-expr is one of
; -- Number
; -- Symbol
; -- (make-add BSL-fun-expr BSL-fun-expr)
; -- (make-mul BSL-fun-expr BSL-fun-expr)
; -- (make-funcall BSL-fun-expr BSL-fun-expr)

(define-struct fundef [name param body])
; A BSL-fun-def is a structure
;   (make-fundef Symbol Symbol BSL-fun-expr)
(define f (make-fundef 'f 'x (make-add 3 'x)))
(define g (make-fundef 'g 'y (make-funcall 'f (make-mul 2 'y))))
(define h (make-fundef 'h 'v (make-add (make-funcall 'f 'v)
                                       (make-funcall 'g 'v))))

; A BSL-fun-def* is an [AL Symbol BSL-fun-def]
; An [AL X Y] is a [List-of [Assocaition X Y]]
; An [Association X Y] is a two element list [List X Y]
(define da-fgh (list (list 'f f) (list 'g g) (list 'h h)))


; eval-function*: BSL-fun-expr BSL-fun-def* -> BSL-value
; Evaluates `bfexpr` in the context of `da`. If `eval-function*` encounters a variable
; that is not in `da`, it signals an error.
(check-expect (eval-function* 1 da-fgh) 1)
(check-expect (eval-function* (make-add 1 (make-mul 3 4)) da-fgh) 13)
(check-expect (eval-function* (make-funcall 'f 5) da-fgh) 8)
(check-expect (eval-function* (make-funcall 'h 2) da-fgh) 12)
(check-error (eval-function* 'x da-fgh))
(check-error (eval-function* (make-funcall 'k (make-add 1 1)) da-fgh))
(define (eval-function* bfexpr da)
  (match bfexpr
    [(? number?) bfexpr]
    [(? symbol?)
     (error "Error: found a symbol in a context that is not a function call")]
    [(add l r) (+ (eval-function* l da) (eval-function* r da))]
    [(mul l r) (* (eval-function* l da) (eval-function* r da))]
    [(funcall n arg)
     (local [(define arg-value (eval-function* arg da))
             (define fun-def (lookup-def da n))
             (define plugd
               (match fun-def [(fundef _n param body) (subst body param arg-value)]))]
       (eval-function* plugd da))]))

; lookup-def: BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of `f` in `da`. Signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(check-error (lookup-def da-fgh 'z))
(define (lookup-def da f)
  (match (assq f da)
    [(list _n def) def]
    [_false (error "Error: " f " is not defined")]))

; subst: BSL-fun-expr Symbol Number -> BSL-var-expr
; Replaces all occurrences of `x` in `bexpr` with `v`
(define (subst bexpr s v)
  (match bexpr
    [(? number?) bexpr]
    [(? symbol?) (if (equal? bexpr s) v bexpr)]
    [(add l r) (make-add (subst l s v) (subst r s v))]
    [(mul l r) (make-mul (subst l s v) (subst r s v))]
    [(funcall n arg) (make-funcall n (subst arg s v))]))
