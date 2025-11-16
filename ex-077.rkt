#lang htdp/bsl

;;; Provide a structure type definition and a data definition for representing points in
;;; time since midnight. A point in time consists of three numbers: hours, minutes, and
;;; seconds.

; a Time is a structure
;   (make-time Number Number Number)
;
; interpretation:
; (make-time h m s) represents a point in time `h` hours, `m` minutes and `s` seconds
; after midnight
;
; Assumptions:
;  hours is a Number in the range [0, 23]
;  minutes is a Number in the range [0, 59]
;  seconds is a Number in the range [0, 59]
(define-struct time [hours minutes seconds])
