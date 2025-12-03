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
