#lang htdp/isl

;;; Explain the following abstract function

; argmax: [X -> Number] [NEList-of X] -> X
; finds the (first) item in `lx` that *maximizes* f
; if (argmax f (list x-1 .. x-n)) == x-i,
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...
;
; (define (argmax f lx) ...)

;;; A:
;;; `argmax` finds the first value in the list for which f(x) is a (local) maximum.
;;;
;;; e.g.
(check-expect (argmax sqr '(1 2 3 -5 4)) -5)
(check-expect (argmax posn-y (list (make-posn 50 1) (make-posn 10 5) (make-posn 30 2)))
              (make-posn 10 5))


;;; Can you articulate an analogous purpose statement for `argmin`?

; argmin: [X -> Number] [NEList-of X] -> X
; finds the (first) item in `lx` that *minimizes* f
; if (argmin f (list x-1 .. x-n)) == x-i,
; then (<= (f x-i) (f x-1)), (<= (f x-i) (f x-2)), ...
;
; (define (argmin f lx) ...)

;;; A:
;;; `argmin` finds the (first) value of `lx` for which f(x) is the lowest value, i.e.
;;; a local minima.

(check-expect (argmin sqrt '(4 25 144)) 4)
(check-expect (argmin string-length '("ab" "c" "def")) "c")
