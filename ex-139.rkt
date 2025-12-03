#lang htdp/bsl

;;; Now take a look at this data definition

; A List-of-numbers is one of:
; -- '()
; -- (cons Number List-of-numbers)

;;; Some elements of this class of data are appropriate inputs for sum from
;;; exercise 138 and some aren't.
;;;
;;; Design the function `pos?`, which consumes a List-of-numbers and determines whether
;;; all numbers are positive numbers. In other words, if `(pos? l)` yields #true, then
;;; l is an element of List-of-amounts. Use DrRacket's stepper to understand how
;;; `pos?` works for `(cons 5 '())` and `(cons -1 '())`.

; List-of-amounts examples
(define loa-empty-example '())
(define loa-ex-1 (cons 1 '()))
(define loa-ex-2 (cons 1 (cons 2 (cons 3 '()))))

; List-of-numbers examples
(define lon-ex-1 (cons 1 (cons 2 '())))
(define lon-ex-2 (cons 1 (cons -2 (cons 0 '()))))

; pos? List-of-numbers -> Boolean
; Determines whether lon is also a List-of-amounts
(check-expect (pos? loa-empty-example) #true)
(check-expect (pos? loa-ex-1) #true)
(check-expect (pos? loa-ex-2) #true)
(check-expect (pos? lon-ex-1) #true)
(check-expect (pos? lon-ex-2) #false)
(define (pos? lon)
  (cond [(empty? lon) #true]
        [(cons? lon)
         (and (positive? (first lon)) (pos? (rest lon)))]))


;;; Also design `checked-sum`. The function consumes a List-of-numbers. It produces their
;;; sum if the input also belongs to List-of-amounts; otherwise it signals an error.

; sum: List-of-amounts -> PositiveNumber
; Adds the amounts in `loa`
(check-expect (sum loa-empty-example) 0)
(check-expect (sum loa-ex-1) 1)
(check-expect (sum loa-ex-2) 6)
(define (sum loa)
  (cond [(empty? loa) 0]
        [(cons? loa)
         (+ (first loa) (sum (rest loa)))]))

; checked-sum: List-of-numbers -> PositiveNumberOrError
; If lon is `pos?`, returns the sum of all its numbers; otherwise it signals an error
(check-expect (checked-sum loa-ex-2) 6)
(check-expect (checked-sum lon-ex-1) 3)
(check-error (checked-sum lon-ex-2))
(define (checked-sum lon)
  (if (pos? lon)
      (sum lon)
      (error "Error: provided a list that has numbers that aren't positive!")))

;;; What does `sum` compute for an element of List-of-numbers?
;;;
;;; It computes a Number, i.e. unlike `sum`, which guarantees we'll get a PositiveNumber,
;;; sum for Numbers will give us numbers, positive _or_ negative (or zero).
