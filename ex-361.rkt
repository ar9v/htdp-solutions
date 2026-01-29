#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `eval-all`. Like `eval-function*` from exercise 359, this function consumes
;;; the representation of an expression and a definitions area. It produces the same value
;;; that DrRacket shows if the expression is entered at the prompt in the interactons area
;;; and the definitions area contains the appropriate definitions.
;;;
;;; HINT: Your `eval-all` function should process variables in the given expression like
;;;       `eval-var-lookup` in exercise 355.

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
(define volume-of-10-cylinder
  (make-fundef 'volume-of-10-cylinder
                      'r
                      (make-mul 10 (make-funcall 'area-of-circle 'r))))

(define area-of-circle
  (make-fundef 'area-of-circle
               'r
               (make-mul 'close-to-pi (make-mul 'r 'r))))

(define triple (make-fundef 'triple 'x (make-mul 3 'x)))

; A BSL-da-all is a [AL Symbol [BSL-fun-def | BSL-fun-expr]]
; An [AL X Y] is a [List-of [Assocaition X Y]]
; An [Association X Y] is a two element list [List X Y]
(define ex-da
  (list
   (list 'volume-of-10-cylinder volume-of-10-cylinder)
   (list 'area-of-circle area-of-circle)
   (list 'close-to-pi 3.14)
   (list 'triple triple)))


; eval-all: BSL-fun-expr BSL-da-all -> [Maybe BSL-value]
; Evaluates `bfexpr` in the context of its definitions area, `da`, if it is possible.
(check-expect (eval-all 1 ex-da) 1)
(check-expect (eval-all 'close-to-pi ex-da) 3.14)
(check-expect (eval-all (make-mul 2 'close-to-pi) ex-da) (* 2 3.14))
(check-expect (eval-all (make-add (make-funcall 'area-of-circle 2)
                                  (make-funcall 'triple 5))
                        ex-da)
              (+ (* 3.14 (* 2 2)) (* 3 5)))
(check-error (eval-all (make-add (make-add 'notfound 'close-to-pi) 3) ex-da))
(check-error (eval-all (make-funcall 'notfunction 3) ex-da))
(check-error (eval-all (make-add 'volume-of-10-cylinder 2) ex-da))
(check-error (eval-all (make-funcall 'close-to-pi 1) ex-da))
(define (eval-all bfexpr da)
  (match bfexpr
    [(? number?) bfexpr]
    [(? symbol?) (lookup-con-def da bfexpr)]
    [(add l r) (+ (eval-all l da) (eval-all r da))]
    [(mul l r) (* (eval-all l da) (eval-all r da))]
    [(funcall f arg)
     (local [(define arg-value (eval-all arg da))
             (define fun-def (lookup-fun-def da f))
             (define plugd
               (match fun-def [(fundef _n param body) (subst body param arg-value)]))]
       (eval-all plugd da))]))

; lookup-con-def: BSL-da-all Symbol -> [Maybe Number]
; looks up `s`'s value in `da`; signals an error if there's no value for it
(define (lookup-con-def da s)
  (match (assq s da)
    [(list s val)
     (if (number? val)
         val
         (error "Error: reference to function " s ", but not as a function call"))]
    [_else (error "Error: " s " is not defined")]))

; lookup-fun-def: BSL-da-all Symbol -> [Maybe BSL-fun-def]
; Looks up the definition of `f` in `da`; signals an error if it can't find it
(define (lookup-fun-def da f)
  (match (assq f da)
    [(list f val)
     (if (number? val)
         (error "Error: " f " is a constant, but expected a function")
         val)]
    [_else (error "Error: " f " is not defined")]))

; subst: BSL-fun-expr Symbol Number -> BSL-var-expr
; Replaces all occurrences of `x` in `bexpr` with `v`
(define (subst bexpr s v)
  (match bexpr
    [(? number?) bexpr]
    [(? symbol?) (if (equal? bexpr s) v bexpr)]
    [(add l r) (make-add (subst l s v) (subst r s v))]
    [(mul l r) (make-mul (subst l s v) (subst r s v))]
    [(funcall n arg) (make-funcall n (subst arg s v))]))
