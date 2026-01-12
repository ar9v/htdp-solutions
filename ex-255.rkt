#lang htdp/isl

;;; Formulate signatures for the following functions:
;;;
;;; -- `map-n`, which consumes a list of numbers and a function for numbers to numbers to
;;;    produce a list of numbers.
;;; -- `map-s`, which consumes a list of strings and a function from strings to strings to
;;;    and produces a list of strings.
;;;
;;; Then abstract over the two signatures, following the above steps. Also show that the
;;; generalized signature can be instantiated to describe the signature of the `map1`
;;; function above.

; map-n: [Number -> Number] [List-of Number] -> [List-of Number]
; map-s: [String -> String] [List-of String] -> [List-of String]
; ->
; map-abs: [X -> X] [List-of X] -> [List-of X]
;
; Here, it turns out that
;   map1: [List-of X] [X -> Y] -> [List-of Y]
;
; is actually more general than `map`! `map-abs` is the instantiation of `map1` when we
; substitute X for Y.
