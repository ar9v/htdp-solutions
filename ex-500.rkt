#lang htdp/isl+

;;; Design an accumulator-style version of `how-many`, which is the function that
;;; determines the number of items on a list. Stop when you have formulated the invariant
;;; and have someone check it.

; how-many: [List-of X] -> N
; Returns the amount of elements in `l`
(check-expect (how-many '()) 0)
(check-expect (how-many '(1 2 3)) 3)
(check-expect (how-many '(1 a "foo" #true)) 4)
(define (how-many l)
  (local [; hm/a: [List-of X] N -> N
          ; Returns the amount of elements in `l`
          ; accumulator a: The size of the difference in elements between `l` and `sl`
          (define (hm/a sl a)
            (cond [(empty? sl) a]
                  [else (hm/a (rest sl) (add1 a))]))]
    (hm/a l 0)))

;;; The performance of `how-many` is `O(n)` where `n` is the length of the list. Does the
;;; accumulator version improve on this?
;;;
;;; A:
;;; Not really, for the same reasons outlined in ex-499.rkt: to count how many elements
;;; there are in a list, we _have_ to go through all of them. The regular versions of
;;; these functions weren't re-processing their intermediate lists.
