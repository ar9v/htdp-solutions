#lang htdp/bsl

;;; Design the `sorted>?` predicate, which consumes a NEList-of-temperatures and produces
;;; `#true` if the temperatures are sorted in descending order. That is, if the second
;;; is smaller than the first, the third smaller than the second and so on. Otherwise
;;; it produces `#false`

; sorted>?: NEList-of-temperatures -> Boolean
; Determines whether `nelot` is sorted in descending order. Returns #false if not.
(check-expect (sorted>? (cons 1 (cons 2 '()))) #false)
(check-expect (sorted>? (cons 3 (cons 2 '()))) #true)
(check-expect (sorted>? (cons 0 (cons 3 (cons 2 '())))) #false)
(define (sorted>? nelot)
  (cond [(empty? (rest nelot)) #true]
        [else
         (and (> (first nelot) (first (rest nelot)))
              (sorted>? (rest nelot)))]))
