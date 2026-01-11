#lang htdp/isl

;;; Argue why the following sentences are now legal:

;; (define (f x) (x 10))
; ISL allows variable names to be used for function application because functions are
; values, so `x` here could be a function value.
;
; `f` is the function that takes a function and calls it with 10 as its argument.

;; (define (f x) (x f))
; Similarly, `x` can be a function value in this context, and we can pass `f` to it since
; `f` is just a value.
;
; `f` is the function that takes a function, and calls it with itself.

;; (define (f x y) (x 'a y 'b))
; Similar idea: `x` can be a function value, and `y` may be any value. We do need to make
; sure that when calling `f`, we pass a function that takes 3 arguments, however.
