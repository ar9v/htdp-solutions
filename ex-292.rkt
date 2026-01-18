#lang htdp/isl+

;;; Design the function `sorted?`, which comes with the following signature and purpose
;;; statement

; sorted?: [X X -> Boolean] [NEList-of X] -> Boolean
; determines whether `l` is sorted according to `cmp`
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)
(check-expect (sorted? > '(3 2 1)) #true)
(define (sorted? cmp l)
  (or (empty? (rest l))
      (and (cmp (first l) (second l))
           (sorted? cmp (rest l)))))
