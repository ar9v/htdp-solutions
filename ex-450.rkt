#lang htdp/isl+

;;; A function `f` is *monotonically increasing* if `(<= (f a) (f b))` holds whenever
;;; `(< a b)` holds. Simplify `find-root` assuming the given function is not only
;;; continuous, but also monotonically increasing.

(define ε 0.001)

; linear: Number -> Number
(define (linear x)
  (- (* 1 x) 4))

; find-root: [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume: `f` is continuous and monotonically increasing
; assume: (<= (f left) 0 (f right))
; generative: divides interval in half, the root is in one of the two halves, picks
; according to assumption
(check-within (find-root linear -10 10) 4 ε)
(define (find-root f left right)
  (local [(define (find-root-acc f l r f@l f@r)
            (cond [(<= (- r l) ε) l]
                  [else
                   (local [(define mid (/ (+ l r) 2))
                           (define f@mid (f mid))]
                     (cond [(positive? f@mid) (find-root-acc f l mid f@l f@mid)]
                           [(negative? f@mid) (find-root-acc f mid r f@mid f@r)]))]))]
    (find-root-acc f left right (f left) (f right))))
