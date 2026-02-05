#lang htdp/isl+

;;; Design `take`. It consumes a list `l` and a natural number `n`. It produces the first
;;; `n` items from `l` or all of `l` if it is too short.

; take: [List-of X] N -> [List-of X]
; Returns the list made up of the first `n` elements from `l`
(check-expect (take '() 0) '())
(check-expect (take '() 2) '())
(check-expect (take '(1 2) 0) '())
(check-expect (take '(1 2) 1) '(1))
(check-expect (take '(1 2 3) 4) '(1 2 3))
(define (take l n)
  (cond [(or (empty? l) (zero? n)) '()]
        [else (cons (first l) (take (rest l) (sub1 n)))]))

;;; Design `drop`. It consumes a list `l` and a natural number `n`. Its result is `l` with
;;; the first `n` items removed or just '() if `l` is too short.

; drop: [List-of X] N -> [List-of X]
; Returns the list made by removing the first `n` items from `l`
(check-expect (drop '() 0) '())
(check-expect (drop '() 3) '())
(check-expect (drop '(1 2) 0) '(1 2))
(check-expect (drop '(1 2 3) 1) '(2 3))
(check-expect (drop '(1 2 3) 4) '())
(define (drop l n)
  (cond [(or (empty? l) (zero? n)) l]
        [else (drop (rest l) (sub1 n))]))
