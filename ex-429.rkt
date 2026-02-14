#lang htdp/isl+

;;; Use `filter` to define `smallers` and `largers`

; largers: [List-of Number] Number -> [List-of Number]
; Produces a list of elements from `alon` that are strictly larger than `n`
(define (largers alon n) (filter (λ (x) (> x n)) alon))

; smallers: [List-of Number] Number -> [List-of Number]
; Produces a list of elements from `alon` that are strictly smaller than `n`
(define (smallers alon n) (filter (λ (x) (< x n)) alon))
