#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `depth`. The function consumes an S-expression and determines its depth. An
;;; Atom has a depth of 1. The depth of a list of S-expressions is the maximum depth of
;;; its items plus 1.

; atom?: S-exp -> Boolean
(define (atom? s-exp)
  (or (number? s-exp) (string? s-exp) (symbol? s-exp)))

; depth: S-exp -> N
; Determines the depth of `sexp`
(check-expect (depth "hello") 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '(world (hello))) 3)
(check-expect (depth '(world (hello (world)) (hello))) 4)
(define (depth sexp)
  (local [(define (depth-sl sl)
            (match sl
              ['() 1]
              [(cons sx rst) (max (depth sx) (depth-sl rst))]))]
    (cond [(atom? sexp) 1]
          [else (add1 (depth-sl sexp))])))
