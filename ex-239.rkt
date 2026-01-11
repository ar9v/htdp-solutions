#lang htdp/isl

;;; A list of two items is another frequently used form of data in ISL programming. Here is
;;; a data definition with two parameters:

; A [List X Y] is a structure:
;   (cons X (cons Y '()))

;;; Instantiate this definition to describe the following classes of data. Add a concrete
;;; example for each:

;; pairs of numbers
; A Pair-of-Numbers is a [List Number Number]
;
; (cons Number (cons Number '()))
(define number-pair (list 1 2))

;; pairs of numbers and 1Strings
; A Pair-of-Number-1String is a [List Number 1String]
;
; (cons Number (cons 1String '()))
(define indexed-string (list 1 "Asia: Heat of the Moment")) ; see what I did there?

;; pairs of Strings and Booleans
; A Pair-of-String-Boolean is a [List String Boolean]
;
; (cons String (cons Boolean '()))
(define property (list "Compressed?" #false))
