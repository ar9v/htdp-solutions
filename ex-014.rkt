#lang htdp/bsl

;; Define the function `string-last`, which extracts the last 1String from a non-empty
;; string.
(define (string-last s)
  (string-ith s (sub1 (string-length s))))
