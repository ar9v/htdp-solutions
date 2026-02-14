#lang htdp/isl+

;;; Develop a variant of `quick-sort<` that uses only one comparison function, say, `<`.
;;; Its partitioning step divides the list the given list `alon` into a list that
;;; contains the items of `alon` smaller than the pivot and another one with those that
;;; are not smaller.
;;;
;;; Use `local` to package up the program as a single function. Abstract this function
;;; so that it consumes a list and a comparison function.

; quick-sort: [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; Returns a sorted variant of `alon`, using `cmp` as the comparison function
(check-expect (quick-sort '(1 4 2 5 3) <) '(1 2 3 4 5))
(check-expect (quick-sort '(1 3 3 1 2 5 4 5 4 5 3) <) '(1 1 2 3 3 3 4 4 5 5 5))
(check-expect (quick-sort '(1 4 2 5 3) >) '(5 4 3 2 1))
(check-expect (quick-sort '(1 3 3 1 2 5 4 5 4 5 3) >) '(5 5 5 4 4 3 3 3 2 1 1))
(check-expect (quick-sort '(1 4 2 5 3) <=) '(1 2 3 4 5))
(check-expect (quick-sort '(1 3 3 1 2 5 4 5 4 5 3) <=) '(1 1 2 3 3 3 4 4 5 5 5))
(check-expect (quick-sort '(1 4 2 5 3) >=) '(5 4 3 2 1))
(check-expect (quick-sort '(1 3 3 1 2 5 4 5 4 5 3) >=) '(5 5 5 4 4 3 3 3 2 1 1))
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
