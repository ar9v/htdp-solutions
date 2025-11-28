#lang htdp/bsl

;;; Design predicates for the following data definitions from the preceding section:
;;; -- SIGS
;;; -- Coordinate (Exercise 105)
;;; -- VAnimal


(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
; A SIGS is one of:
; -- (make-aim UFO Tank)
; -- (make-fired UFO Tank Missile)

; sigs?: Any -> Boolean
; Determines whether `v` is a sigs according to our data definition
;
; (In a revised version, SIGS is a structure, but `define-struct` defines a predicate;
; which is why I chose this interpretation for this exercise)
(define (sigs? v)
  (or (aim? v) (fired? v)))


; A Coordinate is one of:
; -- a NegativeNumber
; -- a PositiveNumber
; -- a Posn

; coordinate?: Any -> Boolean
; Checks if `v` is a Coordinate, i.e. a NegativeNumber, a PositiveNumber or a Posn
(define (coordinate? v)
  (or (posn? v)
      (and (number? v) (or (positive? v) (negative? v)))))


(define-struct vcat [x direction happiness])
(define-struct vcham [x color happiness])
; a VAnimal is either
; -- a VCat
; -- a VCham

; vanimal?: Any -> Boolean
; Determines whether `v` is a VAnimal
(define (vanimal? v)
  (or (vcat? v) (vcham? v)))
