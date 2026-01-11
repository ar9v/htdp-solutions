#lang htdp/isl

;;; Here is one more parametric data definition:

; A [Maybe X] is one of:
; -- #false
; -- X

;;; Interpret these data definitions: [Maybe String], [Maybe [List-of String]], and
;;; [List-of [Maybe String]]

; [Maybe String] is one of
; -- #false
; -- String

; [Maybe [List-of String]] is one of
; -- #false
; -- [List-of String]

; [List-of [Maybe String]] is one of
; -- '()
; -- (cons [Maybe String] [List-of [Maybe String]])

;;; What does the following function signature mean? Work through the remaining steps
;;; of the design recipe.
;;;
;;; A: `occurs` will try to find a given string in a list, but it may not be there(!)
;;;    Instead of signaling an error, it returns false, the alternative in the Maybe
;;;    data definition.

; occurs: String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of `los` starting with `s`; #false otherwise
(check-expect (occurs "a" (list "b" "a" "d" "e")) (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #false)
(define (occurs s los)
  (cond [(empty? los) #false]
        [(cons? los) (if (string=? s (first los))
                         (rest los)
                         (occurs s (rest los)))]))
