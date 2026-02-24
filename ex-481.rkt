#lang htdp/isl+

;;; The tests in figure 175 are awful. No real-world programmer ever spells out all these
;;; possible outcomes.

;;; One solution is to use property testing again. Design the `n-queens-solution?`
;;; function, which consumes a natural number `n` and produces a predicate on queen
;;; placements that determines whether a given placement is a solution to an `n` queens
;;; puzzle:
;;;
;;; -- A solution for an `n` queens puzzle must have length `n`
;;; -- A QP on such a list may not threaten any other, distinct, QP.
;;;
;;; Once you have tested this predicate, use it and `check-satisfied` to formulate the
;;; tests for `n-queens`.

(check-satisfied (n-queens 4) (n-queens-solution? 4))
(check-satisfied (n-queens 5) (n-queens-solution? 5))
(define (n-queens n)
  #false)

; n-queens-solution?: N -> [[List-of QP] -> Boolean]
; Returns a function that checks whether its argument is a valid solution to the n-queens
; problem, for the value `n`.
(define (n-queens-solution? n)
  (λ (qps)
    (and (list? qps)
         (= (length qps) n)
         (andmap (λ (qp1)
                   (andmap (λ (qp2) (or (equal? qp1 qp2)
                                        (not (threatening? qp1 qp2))))
                           qps))
                 qps))))

(define (threatening? qp1 qp2)
  (local [(define (same-row? p1 p2) (= (posn-x p1) (posn-x p2)))
          (define (same-col? p1 p2) (= (posn-y p1) (posn-y p2)))
          (define (same-diagonal? p1 p2)
            (or (= (+ (posn-x p1) (posn-y p1)) (+ (posn-x p2) (posn-y p2)))
                (= (- (posn-x p1) (posn-y p1)) (- (posn-x p2) (posn-y p2)))))]
    (or (same-row? qp1 qp2)
        (same-col? qp1 qp2)
        (same-diagonal? qp1 qp2))))


;;; An alternative solution is to understand the lists of QPs as sets. If two lists contain
;;; the same QPs in different order, they are equivalent as the figure suggests. Hence,
;;; you could formulate the test for `n-queens` as

; data example: [List-of QP]
(define 4QUEEN-SOLUTION-1
  (list
   (make-posn 0 1) (make-posn 1 3)
   (make-posn 2 0) (make-posn 3 2)))

(define 4QUEEN-SOLUTION-2
  (list
   (make-posn 0 2) (make-posn 1 0)
   (make-posn 2 3) (make-posn 3 1)))

; is-queens-result?: [List-of QP] -> Boolean
; is the result equal [as a set] to one of two lists
(define (is-queens-result? x)
  (or (set=? 4QUEEN-SOLUTION-1 x)
      (set=? 4QUEEN-SOLUTION-2 x)))

;;; Design the function `set=?`. It consumese two lists and determines whether they contain
;;; the same items -- regardless of order.

; set=?: [List-of X] [List-of X] -> Boolean
; Do `l1` and `l2` contain the same items?
;
; Does _not_ test list length, only membership!
(check-expect (set=? '(1 2 3) '(1 2 3)) #true)
(check-expect (set=? '() '()) #true)
(check-expect (set=? '(1) '()) #false)
(check-expect (set=? '(1 1 2) '(1 2)) #true)
(check-expect
 (set=? (list (make-posn 0 1) (make-posn 2 0) (make-posn 3 2) (make-posn 1 3))
        4QUEEN-SOLUTION-1)
 #true)
(define (set=? l1 l2)
  (and (andmap (λ (e) (member? e l2)) l1)
       (andmap (λ (e) (member? e l1)) l2)))
