#lang htdp/isl

;;; Formulate signatures for the following functions:
;;;
;;; -- `sort-n`, which consumes a list of numbers and a function that consumes two numbers
;;;    (from the list) and produces a Boolean; `sort-n` produces a list of sorted numbers.
;;;
;;; -- `sort-s`, which consumes a list of strings and a function that consumes two strings
;;;    (from the list) and produces a Boolean; `sort-s` produces a sorted list of strings.
;;;
;;; Then abstract over the two signatures, following the above steps. Also show that the
;;; generalized signature can be instantiated to describe the signature of a sort function
;;; for lists of IRs.

; `sort-n`: [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; `sort-s`: [List-of String] [String String -> Boolean] -> [List-of String]

; `sort`: [List-of X] [X X -> Boolean] -> [List-of X]
; -->
; `sort-ir`: [List-of IR] [IR IR -> Boolean] -> [List-of IR]
