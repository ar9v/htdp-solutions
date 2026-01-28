#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design the `numeric?` function. It determines whether a `BSL-var-expr` is also a
;;; BSL-expr. Here we assume that your solution to exercise 345 is the definition for
;;; BSL-var-expr without Symbols.

(define-struct add [left right])
(define-struct mul [left right])
; a BSL-expr is one of
; -- Number
; -- (make-add BSL-var-expr BSL-var-expr)
; -- (make-mul BSL-var-expr BSL-var-expr)
;
; a BSL-var-exp is one of:
; -- Number
; -- Symbol
; -- (make-add BSL-var-expr BSL-var-expr)
; -- (make-mul BSL-var-expr BSL-var-expr)
(define bvexpr1 (make-add 'x 'y))
(define bvexpr2 (make-add (make-add (make-add 1 1) 3) (make-mul 2 3)))
(define bvexpr3 (make-add (make-add (make-add 1 1) 'x) (make-mul 2 3)))
(define bexpr1 2)
(define bexpr2 (make-mul (make-add 3 4) (make-add 5 6)))

; numeric?: BSL-var-expr -> Boolean
; Determines whether BSL-var-expr `bvexpr` is also a BSL-expr, i.e. if it has no symbols
; in it
(check-expect (numeric? bexpr1) #true)
(check-expect (numeric? bexpr2) #true)
(check-expect (numeric? 1) #true)
(check-expect (numeric? 's) #false)
(check-expect (numeric? bvexpr1) #false)
(check-expect (numeric? bvexpr2) #true)
(check-expect (numeric? bvexpr3) #false)
(define (numeric? bvexpr)
  (match bvexpr
    [(? number?) #true]
    [(? symbol?) #false]
    [(add l r) (and (numeric? l) (numeric? r))]
    [(mul l r) (and (numeric? l) (numeric? r))]))
