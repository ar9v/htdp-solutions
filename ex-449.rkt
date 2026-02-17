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
  (local [(define (find-root-acc f l r f@l f@r)
            (cond
              [(<= (- r l) ε) l]
              [else
               (local [(define mid (/ (+ l r) 2))
                       (define f@mid (f mid))]
                 (cond
                   [(or (<= f@l 0 f@mid) (<= f@mid 0 f@l))
                    (find-root-acc f l mid f@l f@mid)]
                   [(or (<= f@mid 0 f@r) (<= f@r 0 f@mid))
                    (find-root-acc f mid r f@mid f@r)]))]))]
    (find-root-acc f left right (f left) (f right))))


;;; How many recomputations of `(f left)` does this design maximally avoid?
;;;
;;; A:
;;; Suppose we pick an interval for a function such that we always stick with `left`.
;;; Then we are spared from recomputing `(f left)` 2 times however many steps it takes
;;; to reach the base case.
