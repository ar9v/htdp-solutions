#lang htdp/isl

;;; Use `bulid-list` to define a function that
;;;
;;; 1. creates the list (list 0 ... (- n 1)) for any natural number n
;;; 2. creates the list (list 1 ... n) for any natural number n
;;; 3. creates the list (list 1 1/2 ... 1/n) for any natural number n
;;; 4. creates the list of the first n even numbers; and
;;; 5. creates a diagnoal square of 0s and 1s; see exercise 262.

; list-0-to-n-1: Number -> [List-of Number]
; creates a list from [0, n)
(check-expect (list-0-to-n-1 3) (list 0 1 2))
(define (list-0-to-n-1 n)
  (build-list n identity))

; list-1-to-n: Number -> [List-of Number]
; Creates a list from [1, n]
(check-expect (list-1-to-n 4) (list 1 2 3 4))
(define (list-1-to-n n)
  (rest (tabulate n identity)))

; list-1-to-1/n: Number -> [List-of Number]
; Creates a list that goes from [1, 1/n]
(check-expect (list-1-to-1/n 4) (list 1 1/2 1/3 1/4))
(define (list-1-to-1/n n)
  (local ((define (inverse n) (/ 1 n)))
    (map inverse (list-1-to-n n))))

; first-n-even: Number -> [List-of Number]
; Produces a list of the first n even numbers
(check-expect (first-n-even 6) (list 0 2 4 6 8 10))
(define (first-n-even n)
  (local ((define (double x) (* x 2)))
    (map double (list-0-to-n-1 n))))

; identityM: Number -> [List-of [List-of Number]]
; Produces an identity matrix with `n` rows.
(check-expect (identityM 1) '((1)))
(check-expect (identityM 3) '((1 0 0) (0 1 0) (0 0 1)))
(define (identityM n)
  (local ((define (build-row i)
            (local ((define (equals-i? x) (if (= x i) 1 0)))
              (build-list n equals-i?))))
    (build-list n build-row)))

;;; Finally, define `tabulate` from exercise 250 using `build-list`

; tabulate: Number [Number -> Number] -> [List-of Number]
; Tabulates `f` from 0 to n, inclusive
;
; constraint: `n` is positive
; note: technically, we'd have to flip the list to keep parity, but you get the idea.
(check-expect (tabulate 3 identity) (list 0 1 2 3))
(check-expect (tabulate 3 sqr) (list 0 1 4 9))
(define (tabulate n f)
  (build-list (add1 n) f))
