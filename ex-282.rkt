#lang htdp/isl+

(define (f-plain x)
  (* 10 x))

(define f-lambda
  (λ (x) (* 10 x)))

;;; Experiment with the above definitions in DrRacket. Also add...

; compare: Number -> Boolean
(define (compare x)
  (= (f-plain x) (f-lambda x)))

;;; ...to the definitions area after renaming the left hand `f` to `f-plain` and the right
;;; hand one to `f-lambda`. Then run

(compare (random 100000))

;;; a few times to make sure the two functions agree on all kinds of inputs
