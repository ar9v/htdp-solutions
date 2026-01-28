#lang htdp/isl+

;;; Extend the data representation of chapter 21.2 to include the application of a
;;; programmer-defined function. Recall that a function application consists of two pieces:
;;; a name and an expression. The former is the name of the function that is applied; the
;;; latter is the argument.
;;;
;;; Represent these expressions:
;;; (k (+ 1 1))
;;; (* 5 (k (+ 1 1)))
;;; (* (i 5) (k (+ 1 1)))
;;;
;;; We refer to this newly defined class of data as `BSL-fun-expr`.

(define-struct add [left right])
(define-struct mul [left right])
(define-struct funcall [name arg])
; a BSL-fun-expr is one of
; -- Number
; -- Symbol
; -- (make-add BSL-fun-expr BSL-fun-expr)
; -- (make-mul BSL-fun-expr BSL-fun-expr)
; -- (make-funcall Symbol BSL-fun-expr)
(make-funcall 'k (make-add 1 1))
(make-mul 5 (make-funcall 'k (make-add 1 1)))
(make-mul (make-funcall 'i 5) (make-funcall 'k (make-add 1 1)))
