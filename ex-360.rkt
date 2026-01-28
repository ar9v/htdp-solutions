#lang htdp/isl+

(require 2htdp/abstraction)

;;; Formulate a data definition for the representation of DrRacket's definitions area.
;;; Concretely, the data representation should work for a sequence that freely mixes
;;; constant definitions and one-argument function definitions. Make sure you can
;;; represent the definitions area consisting of three definitions at the beginning of
;;; this section. We name this class of data BSL-da-all

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

; A BSL-da-all is a [AL Symbol [BSL-fun-def | BSL-fun-expr]]
; An [AL X Y] is a [List-of [Assocaition X Y]]
; An [Association X Y] is a two element list [List X Y]
(define ex-da
  (list
   (list 'volume-of-10-cylinder volume-of-10-cylinder)
   (list 'area-of-circle area-of-circle)
   (list 'close-to-pi 3.14)))


;;; Design the function `lookup-con-def`. It consumes a BSL-da-all `da` and a symbol `x`.
;;; It produces the representation of a constant definition whose name is `x`, if such
;;; a piece of data exists in `da`; otherwise the function signals an error saying that
;;; no such constant definition can be found

; lookup-con-def: BSL-da-all Symbol -> [Maybe Number]
; looks up `s`'s value in `da`; signals an error if there's no value for it
(check-expect (lookup-con-def ex-da 'close-to-pi) 3.14)
(check-error (lookup-con-def ex-da 'volume-of-10-cylinder))
(check-error (lookup-con-def ex-da 'x))
(define (lookup-con-def da s)
  (match (assq s da)
    [(list s val)
     (if (number? val)
         val
         (error "Error: reference to function " s ", but not as a function call"))]
    [_else (error "Error: " s " is not defined")]))

;;; Design the function `lookup-fun-def`. It consumes a BSL-da-all `da` and a symbol `f`.
;;; It produces the representation of a function definition whose name is `f`, if such a
;;; piece of data exists in `da`; otherwise the function signals an error saying that
;;; no such function definition can be found.

; lookup-fun-def: BSL-da-all Symbol -> [Maybe BSL-fun-def]
; Looks up the definition of `f` in `da`; signals an error if it can't find it
(check-expect (lookup-fun-def ex-da 'area-of-circle) area-of-circle)
(check-error (lookup-fun-def (cons (list 'area-of-circle 1) ex-da) 'area-of-circle))
(check-error (lookup-fun-def ex-da 'close-to-pi))
(check-error (lookup-fun-def ex-da 'z))
(define (lookup-fun-def da f)
  (match (assq f da)
    [(list f val)
     (if (number? val)
         (error "Error: " f " is a constant, but expected a function")
         val)]
    [_else (error "Error: " f " is not defined")]))
