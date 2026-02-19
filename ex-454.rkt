#lang htdp/isl+

;;; Design `create-matrix`. The function consumes a number `n` and a list of `n^2` numbers.
;;; It produces an `n` x `n` matrix, for example

(check-expect
 (create-matrix 2 (list 1 2 3 4))
 '((1 2)
   (3 4)))

; create-matrix: N [List-of N] -> [List-of [List-of N]]
; Creates an n x n matrix, using the numbers `lon`.
;
; assumption: (length lon) === n x n
(check-expect (create-matrix 0 '()) '())
(check-expect (create-matrix 1 '(1)) '((1)))
(check-expect (create-matrix 3 '(1 9 3 8 6 5 7 0 9))
              '((1 9 3)
                (8 6 5)
                (7 0 9)))
(define (create-matrix n lon)
  (cond [(empty? lon) '()]
        [else (cons (take lon n) (create-matrix n (drop lon n)))]))

; take: [List-of X] N -> [List-of X]
; Returns the list made up of the first `n` elements from `l`
(define (take l n)
  (cond [(or (empty? l) (zero? n)) '()]
        [else (cons (first l) (take (rest l) (sub1 n)))]))

; drop: [List-of X] N -> [List-of X]
; Returns the list made by removing the first `n` items from `l`
(define (drop l n)
  (cond [(or (empty? l) (zero? n)) l]
        [else (drop (rest l) (sub1 n))]))
