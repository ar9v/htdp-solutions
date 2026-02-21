#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design the function `check-solution`. It consumes an SOE and a Solution. Its result
;;; is #true if plugging the numbers from the Solution for the variables in the Equations
;;; of the SOE produces equal left-hand-side values and right-hand-side values; otherwise
;;; the function produces #false. Use `check-solution` to formulate tests with
;;; `check-satisfied`.
;;;
;;; HINT: Design the function `plug-in` first. It consumes the left-hand side of an
;;; Equation and a Solution and calculates out the value of the left-hand side when the
;;; numbers from the solution are plugged in for the variables.

(define M ; an SOE
  (list (list 2 2  3 10) ; an Equation
        (list 2 5 12 31)
        (list 4 1 -2 1)))

(define S '(1 1 2)) ; a Solution

; check-solution: SOE Solution -> Boolean
; Checks if plugging in `sol` into `soe` yields `(rhs soe)`
(check-expect (check-solution M S) #true)
(check-expect (check-solution M '(1 2 2)) #false)

(define (check-solution soe sol)
  (for/and [(eq soe)] (= (plug-in (lhs eq) sol) (rhs eq))))

; Read: Is S a solution to (hardcoded) M?
(check-satisfied S (λ (sol) (check-solution M sol)))

; Read: is the SOE provided (M here) solved by provided Solution S?
(check-satisfied (list M S)
                 (λ (soe-sol) (check-solution (first soe-sol) (second soe-sol))))

; plug-in: [List-of Number] Solution -> Number
; Calculates the value resulting from plugging in `s` into `lhs`
;
; Assumption: (length lhs) == (length s)
(check-expect (plug-in '(2 3 4) '(1 1 1)) (+ 2 3 4))
(check-expect (plug-in '(3 2 8) '(3 0 6)) (+ 9 0 48))
(define (plug-in lhs s) (foldr + 0 (map * lhs s)))

; lhs: Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e) (reverse (rest (reverse e))))

; rhs: Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e) (first (reverse e)))
