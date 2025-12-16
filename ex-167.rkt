#lang htdp/bsl

;;; Design the function `sum`, which consumes a list of Posns and produces the sum of all
;;; its x-coordinates.

; sum: List-of-Posns -> Number
; computes the sum of all x-coordinates in lop
(check-expect (sum '()) 0)
(check-expect (sum (cons (make-posn 1 2) (cons (make-posn 3 4) empty))) 4)
(check-expect (sum (cons (make-posn 4 4) (cons (make-posn -2 4) empty))) 2)
(define (sum lop)
  (cond [(empty? lop) 0]
        [(cons? lop)
         (+ (posn-x (first lop))
            (sum (rest lop)))]))
