#lang htdp/bsl

;; Define `==>`. The function consumes two Boolean values, call them sunny and friday.
;; Its answer is `#true` if `sunny` is false or `friday` is true.
(define (==> sunny friday)
  (or (not sunny) friday))

(and #true (==> false false))
(and #true (==> false true))
(and #true (==> true true))
(false?    (==> true false))
