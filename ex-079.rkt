#lang htdp/bsl

;;; Create examples for the following data definitions:

; A Color is one of:
; -- "white"
; -- "yellow"
; -- "orange"
; -- "green"
; -- "red"
; -- "blue"
; -- "black"
"white"
"yellow"
"orange"
"green"
"red"
"blue"
"black"

; H is a Number between 0 and 100.
; interpretation: represents a happiness value
;
; The definition does not state that the range is inclusive, so all of these are interior
1
50
99

(define-struct person [fstname lstname male?])
; A Person is a structure:
;   (make-person String String Boolean)
(make-person "Ricardo" "Vela" #true)
(make-person "Sofia" "Vela" #false)

;;; Is it a good idea to use a field name that looks like the name of a predicate?
;;;
;;; A: Maybe not, at least in the context of how much the book has covered up to this
;;; point. We've been introduced to predicates as functions with a signature of
;;; Any -> Boolean; but structure selector functions will expect to deal with the
;;; structure type they're defined for, which is misleading.


(define-struct dog [owner name age happiness])
; A Dog is a structure
;   (make-dog Person String PositiveInteger H)
;
; Add an interpretation to this data definition too.
;
; interpretation:
; (make-dog o name age happiness) represents a dog whose owner's name is `name`, whose
; aged `age` and has a happiness score of `happiness`.
(make-dog "Ricardo" "Txiki" 12 100)

; A Weapon is one of:
; --- #false
; --- Posn
;
; interpretation:
; #false means the missle hasn't been fired yet; a Posn means it is in flight
#false
(make-posn 2 4)
