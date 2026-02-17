#lang htdp/isl+

;;; The `poly` function has two roots. Use `find-root` with `poly and an interval that
;;; contains both roots.

(define ε 0.0001)

; poly: Number -> Number
(define (poly x) (* (- x 2) (- x 4)))

; find-root: [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous
; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two halves, picks
; according to assumption
(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid)))
       (cond
         [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
          (find-root f left mid)]
         [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
          (find-root f mid right)]))]))

;;; If the interval is large enough -- enough to have a midpoint for which (poly mid)
;;; is positive -- `cond` errors out: both `(f left)` and `(f right)` will be on the
;;; same side of the x-axis.
;;;
;;; However, for intervals where the midpoint falls on [2, 4], the algorithm will
;;; terminate (e.g. left = 1, right = 6 will return ~2)
