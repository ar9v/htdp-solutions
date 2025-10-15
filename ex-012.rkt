#lang htdp/bsl

;; Define the function `cvolume`, which accepts the length of a side of an equilateral
;; cube and computes its volume
(define (cvolume l)
  (expt l 3))

;; If you have time, consider defining `csurface`, too.
(define (csurface l)
  (* 6 (sqr l)))
