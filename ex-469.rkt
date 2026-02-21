#lang htdp/isl+

;;; Design the `solve` function. It consumes triangular SOEs and produces a solution
;;;
;;; HINT: Use structural recursion for the design. Start with the design of a function
;;; that solves a single linear equation in `n + 1` variables, given a solution for the
;;; last `n` variables. In general, this function plugs in the values for the rest of
;;; the left-hand side, subtracts the result from the right-hand side, and divides by the
;;; first coefficient. Experiment with this suggestion and the above examples.
;;;
;;; CHALLENGE: Use an existing abstraction and `lambda` to design `solve`.

(define TM
  '((2 2 3 10)
      (3 9 21)
        (1  2)))

(define TM2
  '((2 3  3   8)
     (-8 -4 -12)
        (-5  -5)))

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
