#lang htdp/isl+

;;; When you worked on exercise 430 or 428, you may have produces looping solutions.
;;; Similarly, exercise 434 actually reveals how brittle the termination argument is for
;;; `quick-sort<`. In all cases, the argument relies on the idea that `smallers` and
;;; `largers` produce lists that are maximally as long as the given list, and our
;;; understanding that neither includes the given pivot in the result.
;;;
;;; Based on this explanation, modify the definition of `quick-sort<` so that both
;;; functions receive lists that are shorter than the given one.

; quick-sort: [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; Returns a sorted variant of `alon`, using `cmp` as the comparison function.
;
; Same as ex. 430, where we had already accounted for this.
(define (quick-sort alon cmp)
  (local [(define (pick l p) (filter (λ (x) (cmp x p)) l))
          (define (reject l p) (filter (λ (x) (not (cmp x p))) l))]
    (cond [(empty? alon) '()]
          [else
           (local [(define pivot (first alon))
                   (define rst (rest alon))]
             (append (quick-sort (pick rst pivot) cmp)
                     (list pivot)
                     (quick-sort (reject rst pivot) cmp)))])))
