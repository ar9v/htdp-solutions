#lang htdp/bsl

;;; Translate the examples into tests and make sure they all succeed. Then change the
;;; function in figure 64 so that everyone gets $14 per hour. Now revise the entire program
;;; so that changing the wage for everyone is a single change to the entire programa and
;;; not several.

(define HOURLY-RATE 14)

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(check-expect (wage* '()) '())
(check-expect (wage* (cons 28 '())) (cons (wage 28) '()))
(check-expect (wage* (cons 4 (cons 2 '()))) (cons (wage 4) (cons (wage 2) '())))
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))

; Number -> Number
; computes the wage for h hours of work
(check-expect (wage 28) (* HOURLY-RATE 28))
(check-expect (wage 4) (* HOURLY-RATE 4))
(check-expect (wage 2) (* HOURLY-RATE 2))
(define (wage h)
  (* HOURLY-RATE h))
