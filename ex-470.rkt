#lang htdp/isl+

(require 2htdp/abstraction)

;;; Define `gauss`, which combines the `triangulate` function from exercise 468 and the
;;; `solve` function from exercise 469.

(define ERR "Error: cannot triangulate an SOE that has 0 for all leading coefficients.")

(define M
  (list (list 2 2  3 10)
        (list 2 5 12 31)
        (list 4 1 -2 1)))

(define M2
  '((2  3  3  8)
    (2  3 -2  3)
    (4 -2  2  4)))

(define bad-M
  '((2 2 2 6)
    (2 2 4 8)
    (2 2 1 2)))

(define TM
  '((2 2 3 10)
    (3 9 21)
    (1  2)))

(define TM2
  '((2 3  3   8)
     (-8 -4 -12)
        (-5  -5)))

; gauss: SOE -> Solution
; Solves the `soe` by combining `triangulate` with `solve`
(check-satisfied (list M (gauss M)) soe-solved-by?)
(check-satisfied (list M2 (gauss M2)) soe-solved-by?)
(define (gauss soe) (solve (triangulate soe)))

(define (check-solution soe sol)
  (for/and [(eq soe)] (= (plug-in (lhs eq) sol) (rhs eq))))

(define soe-solved-by?
  (λ (soe-sol) (match soe-sol [(list soe sol) (check-solution soe sol)])))

; solve: TM -> [List-of Number]
; Produces the solution to a given triangular SOE.
(check-expect (solve TM) '(1 1 2))
(check-expect (solve TM2) '(1 1 1))
(define (solve tm)
  (local [(define (solve-for eqt sol)
            (/ (- (rhs eqt) (plug-in (rest (lhs eqt)) sol)) (first eqt)))]
    (foldr (λ (eqt sols) (cons (solve-for eqt sols) sols)) '() tm)))

; plug-in: [List-of Number] Solution -> Number
; Calculates the value resulting from plugging in `s` into `lhs`
;
; Assumption: (length lhs) == (length s)
(check-expect (plug-in '(2 3 4) '(1 1 1)) (+ 2 3 4))
(check-expect (plug-in '(3 2 8) '(3 0 6)) (+ 9 0 48))
(define (plug-in lhs s) (foldr + 0 (map * lhs s)))

; lhs: Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first TM)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))

; rhs: Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first TM)) 10)
(define (rhs e)
  (first (reverse e)))

; triangulate: SOE -> TM
; triangulates the given system of equations
(check-expect (triangulate '((1 2))) '((1 2)))
(check-expect (triangulate M) TM)
(check-expect (triangulate M2) TM2)
(check-error (triangulate bad-M) ERR)
(define (triangulate M)
  (cond [(empty? (rest M)) M]
        [(andmap zero? (map first M)) (error ERR)]
        [else
         (local [(define valid-M (swap-until (λ (l) (not (zero? (first (first l))))) M))
                 (define eqt1 (first valid-M))]
           (cons eqt1 (triangulate (map (λ (eqt) (subtract eqt eqt1)) (rest valid-M)))))]))

; subtract: Equation Equation -> Equation
; Subtracts a multiple of `e2` from `e1`, item by item, so as to produce a 0 in the first
; position; returns the non-zero coefficients of the result.
(check-expect (subtract '(3 4) '(1 2)) '(-2))
(check-expect (subtract '(8 3 20) '(2 4 16)) '(-13 -44))
(check-expect (subtract '(-20 4 -18) '(4 2 -3)) '(14 -33))
(check-expect (subtract '(7 2 10) '(2 3 8)) '(-17/2 -18))
(define (subtract e1 e2)
  (local [(define k (/ (first e1) (first e2)))]
    (rest (map - e1 (map (scale k) e2)))))

; scale: Number -> [Number -> Number]
; Returns a function that will multiply a given number by `n`
(define (scale n) (λ (x) (* n x)))

; swap-until: [[NEList-of X] -> Boolean] [NEList-of X] -> [NEList-of X]
; Rotates non-empty list `l` until the given `pred` is true. Returns the original list
; if `pred` is never true for any configuration (i.e. after (length l) swaps)
(check-expect (swap-until (λ (l) (even? (first l))) '(1 3 4 6)) '(4 6 1 3))
(check-expect (swap-until (λ (l) (zero? (second l))) '(1 1 1 0)) '(1 0 1 1))
(check-expect (swap-until (λ (l) (even? (first l))) '(5 7 3 1)) '(5 7 3 1))
(define (swap-until pred l)
  (local [(define (swap-until-acc lst swaps)
            (cond [(= swaps (length l)) l]
                  [(pred lst) lst]
                  [else (swap-until-acc (append (rest lst) (list (first lst)))
                                        (add1 swaps))]))]
    (swap-until-acc l 0)))
