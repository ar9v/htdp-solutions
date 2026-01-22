#lang htdp/isl+

;;; Define the `atom?` function
(define (atom? s-exp)
  (or (number? s-exp) (string? s-exp) (symbol? s-exp)))
