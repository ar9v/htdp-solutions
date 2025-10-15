#lang htdp/bsl

;; Define the function `string-first`, which extracts the first 1String from a non-empty
;; string.
(define (string-first s)
  (string-ith s 0))
