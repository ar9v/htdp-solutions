#lang htdp/isl+

;;; Consider the following function definition:

; poly: Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

;;; It defines a binomial for which we can determine its roots by hand

(check-expect (poly 2) 0)
(check-expect (poly 4) 0)

;;; Use `poly` to formulate a `check-satisfied` test for `find-root`

; random-between: Number Number -> Number
; Produces a random number lying between `a` and `b` (i.e. range [a, b))
(check-satisfied (random-between 3 7) (λ (res) (< 2 res 7)))
(define (random-between a b) (+ a (random (- b a))))

; random-bounds: Number Number [Number -> Number] -> [List Number Number]
; Produces a list of two bounds, left/right, for which (f left) and (f right)
; are on opposite sides of the x axis.
;
; It does this by producing `left` and right by generating a random number between
; `low` and `hi` and checking if the pair fulfills the constraints of `find-root`.
;
; Assumes `f` has a root
(define (random-bounds low hi f)
  (local [(define (checked lc hc)
            (if (or (<= (f lc) 0 (f hc))
                    (<= (f hc) 0 (f lc)))
                (list lc hc)
                (random-bounds low hi f)))]
    (checked (random-between low hi) (random-between low hi))))

; find-root: [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous
; (2) (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative: divides interval in half, the root is in one of the two halves, picks
; according to (2)
(define bounds (random-bounds -100 100 poly))
(define left (first bounds))
(define right (second bounds))
(check-satisfied (find-root poly left right)
                 (λ (root) (zero? (poly (round root))))) ; as long as ε isn't too large
(define (find-root f left right)
  0)

;;; Also use `poly` to illustrate the root-finding process. Start with the interval
;;; [3, 6] and tabulate the information as follows for ε = 0

; | step | left |  f left | right | f right |   mid |    f mid |
; |------+------+---------+-------+---------+-------+----------|
; | n=1  |    3 |      -1 |  6.00 |    8.00 |  4.50 |     1.25 |
; | n=2  |    3 |      -1 |  4.50 |    1.25 |  3.75 |  -0.4375 |
; | n=3  | 3.75 | -0.4375 |  4.50 |    1.25 | 4.125 | 0.265625 |
; | ...  |      |         |       |         |       |          |
