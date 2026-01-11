#lang htdp/isl

;;; Functions are values: arguments, results, items in lists. Place the following
;;; definitions and expressions into DrRacket's definitions window and use the stepper to
;;; find out how running this program works:

(define (f x) x)
(cons f '())                            ; => (list function:f)
(f f)                                   ; => function:f
(cons f (cons 10 (cons (f 10) '())))    ; => (list function:f 10 10)
