#lang htdp/isl+

;;; Decide which of the following phrases are legal `lambda` expressions. Explain why they
;;; are legal or illegal. If in doubt, experiment in the interactions area of DrRacket.

; 1. (lambda (x y) (x y y))
;; Legal, this is the function which takes a binary function and applies it to its second
;; argument, which is used in the place of both parameters. E.g.
(check-expect ((lambda (x y) (x y y)) + 2) 4)
(check-expect ((lambda (x y) (x y y)) string-append "boo") "booboo")

; 2. (lambda () 10)
;; Illegal, since lambda requires at least one variable (at least in ISL+)
(check-error (lambda () 10))

; 3. (lambda (x) x)
;; Legal, also known as the `identity` function
(check-random ((lambda (x) x) (random 100)) (random 100))

; 4. (lambda (x y) x)
; This is legal, it is a function that "picks" its first argument. It is also known as the
; K combinator.
(check-expect ((lambda (x y) x) 4 5) 4)

; 5. (lambda x 10)
; Illegal, since it's not syntactically valid. `lambda` requires its second argument to
; be a sequence of variables (at least in ISL+)
(check-error (lambda x 10))
