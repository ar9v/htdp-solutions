#lang htdp/isl

;;; Compare the definitions for NEList-of-temperatures and NEList-of-Booleans. Then
;;; formulate an abstract data definition NEList-of

; A NEList-of-Temperatures is one of
; -- (cons Temperature '())
; -- (cons Temeprature NEList-of-temperatures)

; A NEList-of-Booleans is one of
; -- (cons Boolean '())
; -- (cons Boolean NEList-of-Booleans)

; -->

; A [NEList-of T] is one of
; -- (cons T '())
; -- (cons T [NEList-of T])
