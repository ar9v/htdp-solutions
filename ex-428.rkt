#lang htdp/isl+

;;; If the input to `quick-sort<` contains the same number several times, the algorithm
;;; returns a list that is strictly shorter than the input. Why? Fix the problem so that
;;; the output is as long as the input.
;;;
;;; A:
;;; That's the case because we pick out numbers from the list that are strictly larger
;;; and strictly smaller than a given iteration's pivot.

; quick-sort<: [List-of Number] -> [List-of Number]
; Produces a sorted variant of `alon`
(check-expect (quick-sort< '(1 4 2 5 3)) '(1 2 3 4 5))
(check-expect (quick-sort< '(1 3 3 1 2 5 4 5 4 5 3)) '(1 1 2 3 3 3 4 4 5 5 5))
(define (quick-sort< alon)
  (cond [(or (empty? alon) (empty? (rest alon))) alon]
        [else
         (local [(define pivot (first alon))]
           (append (quick-sort< (filter (λ (x) (<= x pivot)) (rest alon)))
                   (list pivot)
                   (quick-sort< (filter (λ (x) (> x pivot)) (rest alon)))))]))
