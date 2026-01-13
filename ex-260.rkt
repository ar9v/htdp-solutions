#lang htdp/isl

;;; Confirm the insight about the performance of `inf.v2` by repeating the performance
;;; experiment of exercise 238.

; inf's worst case scenario
(define l1
  (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))

; Nelon -> Number
; determines the smallest number on l
(define (inf l)
  (cond [(empty? (rest l)) (first l)]
        [else
         (if (< (first l) (inf (rest l)))
             (first l)
             (inf (rest l)))]))

; Nelon -> Number
; determines the smallest number on l
(define (inf.v2 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf.v2 (rest l))))
       (if (< (first l) smallest-in-rest)
           (first l)
           smallest-in-rest))]))

; cpu time: 20700 real time: 20787 gc time: 37
(time (inf l1))

; cpu time: 0 real time: 0 gc time: 0
(time (inf.v2 l1))
