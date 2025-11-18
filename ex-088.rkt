#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Define a structure type that keeps track of the cat's x-coordinate and its happiness.
;;; Then formulate a data definition for cats, dubbed VCat, including an interpretation.

(define MAX-HAPPINESS 100)

; Happiness is a Number
; interpretation: Happiness represents the happiness "level" or score.
; constraints: It is a number in [0, MAX-HAPPINESS]

(define-struct vcat [x happiness])
; A VCat (Virtual Cat) is a structure:
;   (make-vcat Number Happiness)
;
; interpretation:
; (make-vcat x h) represents a virtual cat whose x-coordinate is `x` (at the center),
; and that has a happiness score of `h`.

(define cat1 (make-vcat 0 100))
(define cat2 (make-vcat 5 50))
