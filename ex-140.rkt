#lang htdp/bsl

;;; Design the function `all-true`, which consumes a list of Boolean values and determines
;;; whether all of them are #true. In other words, if there is any #false on the list,
;;; the function produces #false.

; all-true: List-of-booleans -> Boolean
; True if all values are #true, #false otherwise
(check-expect (all-true '()) #true)
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #true (cons #false (cons #true '())))) #false)
(define (all-true bs)
  (cond [(empty? bs) #true]
        [(cons? bs)
         (and (first bs) (all-true (rest bs)))]))

;;; Now design `one-true`, a function that consumes a list of Boolean values and determines
;;; whether at least one item on the list is #true.

; one-true: List-of-booleans -> Boolean
; Returns true if at least one element of `bs` is #true
(check-expect (one-true '()) #false)
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false (cons #false (cons #true (cons #false '()))))) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)
(define (one-true bs)
  (cond [(empty? bs) #false]
        [else
         (or (first bs) (one-true (rest bs)))]))
