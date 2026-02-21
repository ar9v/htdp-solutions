#lang htdp/isl+

(require 2htdp/abstraction)

;;; Check that the following system of equations
;;;
;;; 2x + 2y + 3z = 10
;;;      3y + 9z = 21
;;;           1z = 2
;;;
;;; has the same solution as the one labeled with ✝.
;;;
;;; Do so by hand and with `check-solution` from exercise 462.

;;; By hand:
;;;
;;; 2(1) + 2(1) + 3(2) = 10
;;;        3(1) + 9(2) = 21
;;;               1(2) = 2
;;;
;;; 2 + 2 +  6 = 10
;;;     3 + 18 = 21
;;;          2 = 2
;;;
;;; 10 = 10
;;; 21 = 21
;;;  2 = 2

;;; With `check-solution`
(define M ; an SOE
  (list (list 2 2  3 10) ; an Equation
        (list 2 5 12 31)
        (list 4 1 -2 1)))

(define ΔM
  '((2 2 3 10)
    (0 3 9 21)
    (0 0 1  2)))

(define S '(1 1 2)) ; a Solution

; check-solution: SOE Solution -> Boolean
; Checks if plugging in `sol` into `soe` yields `(rhs soe)`
(check-expect (check-solution M S) #true)
(check-expect (check-solution M '(1 2 2)) #false)
(check-satisfied S (λ (sol) (check-solution M sol)))
(check-satisfied (list M S) soe-solved-by?)
(check-satisfied (list ΔM S) soe-solved-by?)
(define (check-solution soe sol)
  (for/and [(eq soe)] (= (plug-in (lhs eq) sol) (rhs eq))))

(define soe-solved-by?
  (λ (soe-sol) (match soe-sol [(list soe sol) (check-solution soe sol)])))

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
