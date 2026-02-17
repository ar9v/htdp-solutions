#lang htdp/isl+

;;; Add the test from exercise 445 to the program in figure 159. Experiment with different
;;; values for ε

(define ε 0.0001)

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

; poly: Number -> Number
(define (poly x) (* (- x 2) (- x 4)))

; find-root: [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous
; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two halves, picks
; according to assumption
(define bounds (random-bounds -100 100 poly))
(define left (first bounds))
(define right (second bounds))
(check-satisfied (find-root poly left right)
                 (λ (root) (zero? (poly (round root))))) ; as long as ε isn't too large
(define (find-root f left right)
  (cond
    [(<= (- (max left right) (min left right)) ε) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid)))
       (cond
         [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
          (find-root f left mid)]
         [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
          (find-root f mid right)]))]))
