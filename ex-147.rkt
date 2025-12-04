#lang htdp/bsl

;;; Develop a data definition for NEList-of-Booleans, a representation of non-empty lists
;;; of Boolean values. Then redesign the functions `all-true` and `one-true` from
;;; exercise 140.

; a NEList-of-Booleans is either
; -- (cons Boolean '())
; -- (cons Boolean NEList-of-Booleans)


; all-true: NEList-of-Booleans -> Boolean
; returns #true only if all elements of `nebs` are #true
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #true (cons #false (cons #true '())))) #false)
(define (all-true nebs)
  (cond [(empty? (rest nebs)) (first nebs)]
        [else (and (first nebs) (all-true (rest nebs)))]))

; one-true
; returns #true if at least one element of `nebs` is #true
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false (cons #false (cons #true (cons #false '()))))) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)
(define (one-true nebs)
  (cond [(empty? (rest nebs)) (first nebs)]
        [else (or (first nebs) (one-true (rest nebs)))]))
