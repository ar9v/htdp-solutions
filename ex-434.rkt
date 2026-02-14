#lang htdp/isl+

;;; Consider the following definition of `smallers`, one of the two "problem generators"
;;; for `quick-sort<`:

; smallers: [List-of Number] Number -> [List-of Number]
(define (smallers l n)
  (cond [(empty? l) '()]
        [else (if (<= (first l) n)
                  (cons (first l) (smallers (rest l) n))
                  (smallers (rest l) n))]))

;;; What can go wrong when this version is used with the `quick-sort<` definition from
;;; chapter 25.2?

; quick-sort<: [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))

; largers: [List-of Number] Number -> [List-of Number]
(define (largers l n) (filter (λ (x) (> x n)) l))


;;; A:
;;; `quick-sort<` (as written in chapter 25.2) passes in `alon` to `smallers`, but
;;; `smallers` as defined here uses `<=` to compare. That means the list never shrinks,
;;; because we'll always be including the pivot! So any call to `quick-sort<` will loop
;;; forever.
