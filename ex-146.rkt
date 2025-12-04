#lang htdp/bsl

;;; Design `how-many` for NEList-of-temperatures. Doing so completes `average`, so ensure
;;; that `average` passes all of its tests, too.

; how-many: NEList-of-temperatures -> Number
; Counts how many temperatures there are in `nelot`.
(check-expect (how-many (cons 1 '())) 1)
(check-expect (how-many (cons 1 (cons 2'()))) 2)
(check-expect (how-many (cons 4 (cons 1 (cons 2'())))) 3)
(define (how-many nelot)
  (cond [(empty? (rest nelot)) 1]
        [else
         (+ 1 (how-many (rest nelot)))]))
