#lang htdp/isl+

;;; Reformulate the data definition for S-expr so that the first clause is expanded into
;;; the three clauses of Atom and the second clause uses the List-of abstraction.

; A S-expr is one of
; -- Number
; -- Symbol
; -- String
; -- [List-of S-expr]

;;; Redesign the `count` function for this data definition.

; count.v1: S-expr Symbol -> Number
; counts all occurrences of sy in sexp
(check-expect (count.v1 'world 'hello) 0)
(check-expect (count.v1 '(world hello) 'hello) 1)
(check-expect (count.v1 '(((world) hello) hello) 'hello) 2)
(define (count.v1 sexp sym)
  (cond [(or (number? sexp) (string? sexp)) 0]
        [(symbol? sexp) (if (equal? sexp sym) 1 0)]
        [(empty? sexp) 0]
        [else (+ (count.v1 (first sexp) sym) (count.v1 (rest sexp) sym))]))

;;; Now integrate the definition of SL into the one for S-expr. Simplify `count` again.
;;; Consider using `lambda`.

; A S-expr is one of
; -- Number
; -- Symbol
; -- String
; -- '()
; -- (cons S-expr S-expr)

; count: S-expr Symbol -> Number
; counts all occurrences of sy in sexp
(check-expect (count.v2 'world 'hello) 0)
(check-expect (count.v2 '(world hello) 'hello) 1)
(check-expect (count.v2 '(((world) hello) hello) 'hello) 2)
(define (count.v2 sexp sym)
  (cond [(or (number? sexp) (string? sexp)) 0]
        [(symbol? sexp) (if (equal? sexp sym) 1 0)]
        [else (foldr (λ (sx acc) (+ acc (count.v2 sx sym))) 0 sexp)]))

;;; NOTE: This kind of simplification is not always possible, but experienced programmers
;;; tend to recognize such opportunities
