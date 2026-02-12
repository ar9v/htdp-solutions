#lang htdp/isl+

;;; Articulate purpose statements for `smallers` and `largers` in figure 149.

; largers: [List-of Number] Number -> [List-of Number]
; Produces a list of elements from `alon` that are strictly larger than `n`
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))

; smallers: [List-of Number] Number -> [List-of Number]
; Produces a list of elements from `alon` that are strictly smaller than `n`
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))
