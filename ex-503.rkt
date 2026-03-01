#lang htdp/isl+

;;; Exercise 467 implicitly asks for the design of a function that rotates a Matrix
;;; until the first coefficient of the first row  differs from 0. In the context of
;;; Exercise 467, the solution calls for a generative-recursive function that creates
;;; a new matrix by shifting the first row to the end when it encounters a 0 in the first
;;; position. Here is the solution:

(define ERR-ALL-START-WITH-ZERO "All rows start with 0!")

; rotate: Matrix -> Matrix
; finds a row that doesn't start with 0 and uses it as the first one
; generative: moves the first row to last place
; errors: if all rows in M start with 0
(check-expect (rotate '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-error (rotate '((0 4 5) (0 2 3))) ERR-ALL-START-WITH-ZERO)
(define (rotate M)
  (cond [(andmap (λ (r) (zero? (first r))) M) (error ERR-ALL-START-WITH-ZERO)]
        [(not (= (first (first M)) 0)) M]
        [else (rotate (append (rest M) (list (first M))))]))

;;; Stop! Modify this function so that it signals an error when all rows start with 0

;;; If you measure this function on large instances of Matrix, you get a surprising result:
;;;
;;;  | rows in M | 1000 | 2000 | 3000 | 4000 | 5000 |
;;;  | rotate    |   17 |   66 |  151 |  272 | 436  |
;;;
;;; As the number of rows increases from 1,000 to 5,000, the time spent by rotate does not
;;; increase by a factor of five but by twenty.
;;;
;;; The problem is that rotate uses append, which makes a brand-new list like `(rest M)`
;;; only to add `(first M)` at the end. If M consists of 1,000 rows and the last row is
;;; the only one with a non-0 coefficient, that’s roughly
;;;
;;; 1000 * 1000 = 1,000,000
;;;
;;; lists.
;;;
;;; How many lists do we get if `M` consists of 5,000 lines?
;;;
;;; A: 5,000 ^ 2 lists

;;; Now suppose we conjecture that the accumulator-style version is faster
;;; than the generative one. Here is the accumulator template for a
;;; structurally recursive version of rotate:

; (define (rotate.v2 M0)
;   (local (; Matrix ... -> Matrix
;           ; accumulator ...
;           (define (rotate/a M seen)
;             (cond
;               [(empty? (rest M)) ...] ; Can this be simplified to (empty? M)
;               [else (... (rotate/a (rest M)
;                                    ... seen ...)
;                          ...)])))
;     (rotate/a M0 ...)))

;;; The goal is to remember the first row when its leading coefficient is
;;; 0 without using append for every recursion.

;; Formulate an accumulator statement. Then follow the accumulator design
;; recipe to complete the above function. Measure how fast it runs on a
;; Matrix that consists of rows with leading 0s except for the last one.
;; If you completed the design correctly, the function is quite fast.

; rotate.v2: Matrix -> Matrix
; finds a row that doesn't start with 0 and uses it as the first one
; errors: if all rows in M start with 0
(check-expect (rotate.v2 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-expect (rotate.v2 '((0 4 5) (1 2 3) (0 6 7)))
              '((1 2 3) (0 6 7) (0 4 5)))
(check-expect (rotate.v2 '((0 4 5) (0 6 7) (0 8 9) (1 2 3)))
              '((1 2 3) (0 8 9) (0 6 7) (0 4 5)))
(check-error (rotate.v2 '((0 4 5) (0 2 3) (0 7 8))) ERR-ALL-START-WITH-ZERO)
(define (rotate.v2 M)
  (local [; rotate/a: Matrix ... -> Matrix
          ; accumulator seen: represents the rows in M not in M1 that have a leading 0
          ; coefficient.
          (define (rotate/a M1 seen)
            (cond [(empty? M1) (error ERR-ALL-START-WITH-ZERO)]
                  [(not (zero? (first (first M1)))) (append M1 seen)]
                  [else (rotate/a (rest M1) (cons (first M1) seen))]))]
    (rotate/a M '())))

(define (test-matrix n)
  (build-list n (λ (row) (make-list 4 (if (= row (- n 1)) 1 0)))))

(for-each
 (λ (n)
   (local [(define subject (test-matrix n))]
     (list (time (rotate subject))
           (time (rotate.v2 subject)))))
 (range 100 1000 100))

;; n = 100
;; cpu time: 1 real time: 1 gc time: 0
;; cpu time: 0 real time: 0 gc time: 0

;; n = 200
;; cpu time: 6 real time: 6 gc time: 0
;; cpu time: 0 real time: 0 gc time: 0

;; n = 300
;; cpu time: 13 real time: 13 gc time: 0
;; cpu time: 0 real time: 0 gc time: 0

;; n = 400
;; cpu time: 24 real time: 24 gc time: 1
;; cpu time: 0 real time: 0 gc time: 0

;; n = 500
;; cpu time: 33 real time: 34 gc time: 0
;; cpu time: 0 real time: 0 gc time: 0

;; n = 600
;; cpu time: 54 real time: 55 gc time: 5
;; cpu time: 0 real time: 0 gc time: 0

;; n = 700
;; cpu time: 64 real time: 64 gc time: 0
;; cpu time: 0 real time: 0 gc time: 0

;; n = 800
;; cpu time: 86 real time: 87 gc time: 0
;; cpu time: 0 real time: 0 gc time: 0

;; n = 900
;; cpu time: 103 real time: 104 gc time: 0
;; cpu time: 0 real time: 0 gc time: 0
