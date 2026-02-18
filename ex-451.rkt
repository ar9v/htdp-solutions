#lang htdp/isl+

;;; A table is a structure of two fields: a natural number `VL` and a function `array`,
;;; which consumes natural numbers and, for those between 0 and `VL` (exclusive), produces
;;; answers:

(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

; table-ref: Table N -> Number
; Looks up the ith value in array of `t`
(define (table-ref t i)
  ((table-array t) i))

(define table1 (make-table 3 identity))
(define table2 (make-table 30 (λ (x) (- (* 2 x) 24))))
(define table3 (make-table 30 (λ (x) (+ (* 2 x) 24))))
(define pow-of-two (make-table 10 (λ (x) (- (expt 2 x) 16))))

;;; [...] The root of a table `t` is a number in `(table-array t)` that is close to
;;; 0. A *root index* is a natural number `i` such that `(table-ref t i)` is a root of
;;; table `t`. A table `t` is monotonically increasing if `(table-ref t 0)` is less
;;; than `(table-ref t 1)`, `(table-ref t 1)` is less than `(table-ref t 2)`, and so on.

(define ε 0.001)

; assume: table is not empty (size 0)

;;; Design `find-linear`. The function consumes a monotonically increasing table and
;;; finds the smallest index for a root of the table. Use the structural recipe for N,
;;; proceeding from 0 through 1, 2, and so on to the `array-length` of the given table.
;;; This kind of root-finding process is often called a `linear-search`.

; find-linear: Table -> N
; Determines the table's root index, if it exists.
(check-expect (find-linear table1) 0)
(check-expect (find-linear table2) 12)
(check-expect (find-linear pow-of-two) 4)
(check-error (find-linear table3))
(define (find-linear t)
  (local [(define (find-linear-helper i)
            (cond [(= i vl) (error "No root found")]
                  [(<= (abs (table-ref t i)) ε) i]
                  [else (find-linear-helper (add1 i))]))
          (define vl (table-length t))]
    (find-linear-helper 0)))

;;; Design `find-binary`, which also finds the smallest index for the root of a
;;; monotonically increasing table but uses generative recursion to do so. Like ordinary
;;; binary search, the algorithm narrows down an interval down to the smallest possible
;;; size and then chooses the index. Don't forget to formulate a termination argument.

; find-binary: Table -> N
; Determines the table's root index, if it exists.
;
; generative: Checks the table's value at a midpoint. If the midpoint is the root index,
; returns that. Otherwise, it recurs with a new range half the size of the original
; depending on the table's value at midpoint. Terminates since the range eventually
; becomes of size 0
;
(check-expect (find-binary table1) 0)
(check-expect (find-binary table2) 12)
(check-expect (find-binary pow-of-two) 4)
(check-error (find-binary table3))
(define (find-binary t)
  (local [(define (helper l r)
            (local [(define m (floor (/ (+ l r) 2)))
                    (define t@m (table-ref t m))]
              (cond [(= r l) (error "Root not found")]
                    [(<= (abs t@m) ε) m]
                    [(< 0 t@m) (helper l m)]
                    [else (helper m r)])))]
    (helper 0 (- (table-length t) 1))))

;;; HINT: The key problem is that a talbe index is a **natural** number, not a plain
;;; number. Hence, the interval boundary arguments for `find` must be natural numbers.
;;; Consider how this observation changes (1) the nature of trivially solvable problem
;;; instances, (2) the midpoint computation, (3) and the decision as to which interval to
;;; generate next. To make this concrete, imagine a table with 1024 slots and the root
;;; at 1023. How many calls to `find` are needed in `find-linear` and `find-binary`,
;;; respectively?
