#lang htdp/isl+

;;; Like sum, !.v1 performs the primitive computations, multiplication in
;;; this case, in reverse order. Surprisingly, this affects the
;;; performance of the function in a negative manner.

;;; Measure how long it takes to evaluate (!.v1 20) 1,000 times. Recall
;;; that (time an-expression) function determines how long it takes to run
;;; an-expression.

(define (!.v1 n)
  (cond [(zero? n) 1]
        [else (* n (!.v1 (sub1 n)))]))

(define (!.v2 n0)
  (local (; N N -> N
          ; computes (* n (- n 1) (- n 2) ... 1)
          ; accumulator a is the product of the
          ; natural numbers in the interval [n0,n)
          (define (!/a n a)
            (cond
              [(zero? n) a]
              [else (!/a (sub1 n) (* n a))])))
    (!/a n0 1)))

; NOTE: values for 20 were
;
;   cpu time: 0 real time: 0 gc time: 0
;
; across the board, hence the (big) bump here.
(time (!.v1 10000)) ; cpu time: 75 real time: 76 gc time: 20
(time (!.v2 10000)) ; cpu time: 68 real time: 69 gc time: 0
