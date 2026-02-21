#lang htdp/isl+

;;; Here is a representation for triangular SOEs:

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length
;   n + 1, n, n - 1, ..., 2.
;
; interpretation: represents a triangular matrix

;;; Design the `triangulate` algorithm. Turn the example into a test and spell out explicit
;;; answers for the four questions based on our loose description. Do not yet deal with
;;; the termination step of the design recipe.

;;; 1. What is a trivially solvable problem?
;;;    A SOE with a single equation.
;;;
;;; 2. How are trivial solutions solved?
;;;    A trivially solvable SOE is itself the answer to `triangulate`.
;;;
;;; 3. How does the algorithm generate new problems that are more easily solvable than the
;;;    original one? Is there one new problem that we generate or are there several?
;;;    It (1) subtracts the first equation from the rest in the SOE and (2) triangulates
;;;    the SOE that results from getting the `rest` of this new SOE.
;;;
;;; 4. Is the solution of the given problem the same as the solution of (one of) the new
;;;    problems? And, if so, do we need anything from the original problem data?
;;;    We need to combine the first equation in the original SOE with the result of
;;;    triangulating the rest of the equations, post subtraction.

(define M
  (list (list 2 2  3 10)
        (list 2 5 12 31)
        (list 4 1 -2 1)))

(define TM
  '((2 2 3 10)
      (3 9 21)
        (1  2)))

; triangulate: SOE -> TM
; triangulates the given system of equations
(check-expect (triangulate '((1 2))) '((1 2)))
(check-expect (triangulate M) TM)
(define (triangulate M)
  (cond [(empty? (rest M)) M]
        [else
         (local [(define eqt1 (first M))]
           (cons eqt1 (triangulate (map (λ (eqt) (subtract eqt eqt1)) (rest M)))))]))

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
