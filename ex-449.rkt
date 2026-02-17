#lang htdp/isl+

;;; As presented in figure 159, `find-root` computes the value of `f` for each boundary
;;; value twice to generate the next interval. Use `local` to avoid this recomputation.
;;;
;;; In addition, `find-root` recomputes the value of a boundary accross recursive calls.
;;; for example `(find-root f left right)` computes `(f left)` and, if [left, mid] is
;;; chosen as the next interval, `find-root` computes `(f left)` again. Introduce a
;;; helper function that is like `find-root` but consumes not only `left` and `right` but
;;; also `(f left)` and `(f right)` at each recursive stage.

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
     (local [(define mid (/ (+ left right) 2))
             (define f@mid (f mid))
             (define f@l (f left))
             (define f@r (f right))]
       (cond
         [(or (<= f@l 0 f@mid) (<= f@mid 0 f@l))
          (find-root f left mid)]
         [(or (<= f@mid 0 f@r) (<= f@r 0 f@mid))
          (find-root f mid right)]))]))


;;; How many recomputations of `(f left)` does this design maximally avoid?
;;;
;;; NOTE: the two additional arguments to this helper function change at each recursive
;;; stage, but the change is related to the change in the numeric arguments. These
;;; arguments are so-called `accumulators`, which are the topic of part VI.
