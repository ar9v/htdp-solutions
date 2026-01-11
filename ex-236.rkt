#lang htdp/isl

;;; Create test suites for the following two functions. Then abstract over them. Define
;;; the two functions in terms of the abstraction as one-liners and use the existing
;;; test suites to confirm that the revised definitions work properly. Finally, design
;;; a function that subtracts 2 from each number on a given list.

; add-n/list: Number List<Number> -> List<Number>
; Adds `n` to each item on `l`
(define (add-n/list n l)
  (cond [(empty? l) '()]
        [(cons? l)
         (cons (+ n (first l))
               (add-n/list n (rest l)))]))

; add1*: List<Number> -> List<Number>
; adds 1 to each item on `l`
(check-expect (add1* '()) '())
(check-expect (add1* '(1 4 -9)) '(2 5 -8))
(define (add1* l)
  (add-n/list 1 l))

; plus5: List<Number> -> List<Number>
; adds 5 to each item on `l`
(check-expect (plus5 '()) '())
(check-expect (plus5 '(1 4 -9)) '(6 9 -4))
(define (plus5 l)
  (add-n/list 5 l))

; minus2: List<Number> -> List<Number>
(check-expect (minus2 '()) '())
(check-expect (minus2 '(1 4 -9)) '(-1 2 -11))
(define (minus2 l)
  (add-n/list -2 l))
