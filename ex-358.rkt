#lang htdp/isl+

(require 2htdp/abstraction)

;;; Provide a structure type and a data definition for function definitions. Recall that
;;; such a definition has three essential attributes:
;;;
;;; 1. The function's name, which is represented with a symbol
;;; 2. The function's parameter, which is also a name; and
;;; 3. The function's body, which is a variable expression.
;;;
;;; We use BSL-fun-def to refer to this class of data.
;;;
;;; Use your data definition to represent these BSL function definitions
;;;
;;; 1. (define (f x) (+ 3 x))
;;; 2. (define (g y) (f (* 2 y)))
;;; 3. (define (h v) (+ (f v) (g v)))

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


;;; Next, define the class BSL-fun-def* to represent a definitions area that consists of a
;;; number of one-argument function definitions. Translate the definitions area that
;;; defines `f`, `g`, and `h` into your data representation and name it `da-fgh`.

; A BSL-fun-def* is an [AL Symbol BSL-fun-def]
; An [AL X Y] is a [List-of [Assocaition X Y]]
; An [Association X Y] is a two element list [List X Y]
(define da-fgh (list (list 'f f) (list 'g g) (list 'h h)))


;;; Finally, work on the following wish:

; lookup-def: BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of `f` in `da`. Signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(check-error (lookup-def da-fgh 'z))
(define (lookup-def da f)
  (match (assq f da)
    [(list _n def) def]
    [_false (error "Error: " f " is not defined")]))
