#lang htdp/bsl+

;;; Take a second look at intermezzo 1, the intermezzo that presents BSL and its ways
;;; of formulating tests. One of the latter is `check-satisfied`, which determines
;;; whether an expression satisfies a certain property. Use `sorted>?` from exercise
;;; 145 to reformulate the tests for `sort>` with `check-satisfied`

; The original function in exercise 145 assumes it's given a non-empty list.
; This function is an updated version to account for `sort>`'s first test case.
(define (sorted>? l)
  (cond [(or (empty? l) (empty? (rest l))) #true]
        [else
         (and (> (first l) (first (rest l)))
              (sorted>? (rest l)))]))

; sort>: List<Number> -> List<Number>
; rearranges alon in descending order
(check-satisfied (sort> '()) sorted>?)
(check-satisfied (sort> (list 3 2 1)) sorted>?)
(check-satisfied (sort> (list 1 2 3)) sorted>?)
(check-satisfied (sort> (list 12 20 -5)) sorted>?)
(define (sort> alon)
  (cond [(empty? alon) '()]
        [(cons? alon)
         (insert (first alon) (sort> (rest alon)))]))

; insert: Number List<Number> -> List<Number>
; inserts n into the sorted list of numbers alon
(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 12 (list 20 -5)) (list 20 12 -5))
(define (insert n alon)
  (cond [(empty? alon) (list n)]
        [(cons? alon)
         (if (< n (first alon))
             (cons (first alon) (insert n (rest alon)))
             (cons n alon))]))


;;; Now consider this function definition:

; sort>/bad: List<Number> -> List<Number>
; produces a sorted version of `l`
(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))

;;; Can you formulate a test case that shows that `sort>/bad` is *not* a sorting function?
;;; Can you use `check-satisfied` to formulate this test case?

;;; 1. We can, with `check-expect`
(check-expect (sort>/bad (list 11 12 13)) (list 13 12 11))

;;; 2. We can't, I don't think. Since `check-satisfied` uses the predicate with the result
;;; of evaluating its first argument, the predicate can only assert things about the
;;; result itself. But `sort>/bad` is not a sorting function to the extent that the its
;;; _input_ is not sorted. I.e. a sorting function's spec is contingent on _its_ values
;;; being arranged in order, not just fulfilling the property of its _result_ having
;;; arranged values.
