#lang htdp/bsl

;;; Formulate data definitions for the following structure type definitions. Make sensible
;;; assumptions as to what kind of values go into each field.

; A Movie is a structure
;  (make-movie String String Number)
;
; interpretation:
; (make-movie title producer year) represents a movie titled `title`, whose producer's
; name is `producer`, and which was released in `year`
(define-struct movie [title producer year])

; a Person is a structure
;  (make-person String String String Phone)
;
; interpretation:
; (make-person n h e p) represents a person whose full name is `n`, whose hair color is
; `h`, whose eye color is `e`, and whose phone number is `p`. Here, Phone refers to the
; structure we've defined in previous exercises (e.g. 70 and 72)
(define-struct person [name hair eyes phone])

; a Pet is a structure
;  (make-pet String Number)
;
; interpretation:
; (make-pet n num) represents a pet whose name is `n`, and whose all-time ranking is `num`.
; For example: (make-pet "Txiki" 1), because Txiki is the best pet (and thus doggo) ever.
(define-struct pet [name number])

; a CD is a structure
;   (make-CD String String Number)
;
; interpretation:
; (make-CD a t p) represents a CD released by artist `a`, with title `t` and which is sold
; for `p` dollars, rounded to the nearest dollar.
(define-struct CD [artist title price])

; a Sweater is a structure
;   (make-sweater String String String)
;
; interpretation:
; (make-sweater m s p) represents a sweater made out of material `m`, of some size `s`
; produced by `p`
(define-struct sweater [material size producer])
