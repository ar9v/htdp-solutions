#lang htdp/bsl

;;; No employee could possibly  work more than 100 hours per week. To protect the company
;;; against fraud, the function should check that no item of the input list of `wage*`
;;; exceeds 100. If one of them does, the function should immediately signal an error.
;;; How do we have to change the function in figure 64 if we want to perform this basic
;;; reality check?

(define HOURLY-RATE 14)

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(check-expect (wage* '()) '())
(check-expect (wage* (cons 28 '())) (cons (wage 28) '()))
(check-expect (wage* (cons 4 (cons 2 '()))) (cons (wage 4) (cons (wage 2) '())))
(check-error (wage* (cons 4 (cons 2 (cons 101 (cons 3 '()))))))
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))

; Number -> Number
; computes the wage for h hours of work; throws an error if the hour is greater than 100.
(check-expect (wage 28) (* HOURLY-RATE 28))
(check-expect (wage 4) (* HOURLY-RATE 4))
(check-expect (wage 2) (* HOURLY-RATE 2))
(check-error (wage 101))
(define (wage h)
  (if (<= h 100)
      (* HOURLY-RATE h)
      (error "ERROR: Suspicious activity detected (" h " hours)")))
