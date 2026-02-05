#lang htdp/isl+

;;; Design `merge`. The function consumes two lists of numbers, sorted in ascending order.
;;; It produces a single sorted list of numbers that contains all the numbers on both
;;; inputs lists. A number occurs in the output as many times as it occurs on the two input
;;; lists together.

; merge: [List-of N] [List-of N] -> [List-of N]
; Joins `l1` and `l2` into a new, sorted list
;
; assumption: `l1` and `l2` are sorted
(check-expect (merge '() '()) '())
(check-expect (merge '() '(1 2)) '(1 2))
(check-expect (merge '(1 2) '()) '(1 2))
(check-expect (merge '(1 2 3) '(3 3 4 5 6)) '(1 2 3 3 3 4 5 6))
(check-expect (merge '(6 7 7 8) '(1 3 5 5)) '(1 3 5 5 6 7 7 8))
(define (merge l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [else
         (if (< (first l1) (first l2))
             (cons (first l1) (merge (rest l1) l2))
             (cons (first l2) (merge l1 (rest l2))))]))
