#lang htdp/isl+

;;; Formulate a data definition that replaces the informal "or" signature for the
;;; definition of the `list-of-attributes?` function.

; AttributesOrXexpr is one of
; -- [List-of Attribute]
; -- Xexpr

; list-of-attributes? AttributesOrXexpr -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (or (empty? x) (cons? (first x))))
