#lang htdp/isl

;;; Here are two strange but similar data definitions:

; An LStr is one of:
; -- String
; -- (make-layer LStr)
(define lstr-base "foo")
(define lstr-shallow (make-layer "foo"))
(define lstr-deep (make-layer (make-layer "foo")))

; An LNum is one of:
; -- Number
; -- (make-layer LNum)
(define lnum-base 1)
(define lnum-shallow (make-layer 1))
(define lnum-deep (make-layer (make-layer 1)))

;;; Both data definitions rely on this structure-type definition:
(define-struct layer [stuff])

;;; Both define nested forms of data: one is about numbers and the other about strings.
;;; Make examples for both. Abstract over the two. Then instantiate the abstract definition
;;; to get back to the originals.

; A [Layer T] is one of
; -- T
; -- (make-layer [Layer T])

; An LStr is a [Layer String]
; -- T              -> String
; -- (make-layer T) -> (make-layer [Layer String])

; An LNum is a [Layer Num]
; -- T                      -> Number
; -- (make-layer [Layer T]) -> (make-layer [Layer Num])
