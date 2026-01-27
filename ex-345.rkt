#lang htdp/isl+

;;; Formulate a data definition for the representation of BSL expressions based on the
;;; structure type definitions of `add` and `mul`. Let's use BSL-expr in analogy for
;;; S-expr for the new class of data.

(define-struct add [left right])
(define-struct mul [left right])
; a BSL-expr is one of
; -- Number
; -- (make-add BSL-expr BSL-expr)
; -- (make-mul BSL-expr BSL-expr)

;;; Translate the following expressions into data:

; 1. (+ 10 -10)
(make-add 10 -10)

; 2. (+ (* 20 3) 33)
(make-add (make-mul 20 3) 33)

; 3. (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9)))
(make-add (make-mul 3.14 (make-mul 2 3))
          (make-mul 3.14 (make-mul -1 -9)))

;;; Interpret the following data as expressions:

(make-add -1 2)
; (+ -1 2)

(make-add (make-mul -2 -3) 33)
; (+ (* -2 -3) 33)

(make-mul (make-add 1 (make-mul 2 3)) 3.14)
; (* (+ 1 (* 2 3)) 3.14)
