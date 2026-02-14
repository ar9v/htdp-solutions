#lang htdp/isl+

;;; While `quick-sort<` quickly reduces the size of the problem in many cases, it is
;;; inappropriately slow for small problems. Hence, people use `quick-sort<` to reduce
;;; the size of the problem and switch to a different sort function when the list is
;;; small enough.
;;;
;;; Develop a version of `quick-sort<` that uses `sort<` (an appropriately adapted
;;; variant of `sort>` from chapter 11.3) if the length of the input is below some
;;; threshold.

; quick-sort<: [List-of Number] -> [List-of Number]
; Produces a sorted version of `alon`
; assume all the numbers are distinct
(define (quick-sort< alon)
  (local [(define threshold 10)]
    (cond [(< (length alon) threshold) (sort< alon)]
          [else
           (local [(define pivot (first alon))]
             (append (quick-sort< (smallers alon pivot))
                     (list pivot)
                     (quick-sort< (largers alon pivot))))])))

; sort<: [List-of Number] -> [List-of Number]
; Produces a sorted version of `alon`, but uses the insertion sort algorithm
(define (sort< alon)
  (local [(define (insert x l)
            (cond [(empty? l) (list x)]
                  [else
                   (if (< x (first l))
                       (cons x l)
                       (cons (first l) (insert x (rest l))))]))]
    (foldr insert '() alon)))

; largers: [List-of Number] Number -> [List-of Number]
; Produces a list of elements from `alon` that are strictly larger than `n`
(define (largers alon n) (filter (λ (x) (> x n)) alon))

; smallers: [List-of Number] Number -> [List-of Number]
; Produces a list of elements from `alon` that are strictly smaller than `n`
(define (smallers alon n) (filter (λ (x) (< x n)) alon))
