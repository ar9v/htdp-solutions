#lang htdp/isl+

;;; Revise the algorithm `triangulate` from exercise 466 so that it rotates the equations
;;; first to find one with a leading coefficient that is not 0 before it subtracts the
;;; first equation from the remaining ones.
;;;
;;; HINT: The following expression rotates a non-empty list L:
;;;
;;; (append (rest L) (list (first L)))

(define M
  (list (list 2 2  3 10)
        (list 2 5 12 31)
        (list 4 1 -2 1)))

(define M2
  '((2  3  3  8)
    (2  3 -2  3)
    (4 -2  2  4)))

(define TM
  '((2 2 3 10)
      (3 9 21)
        (1  2)))

(define TM2
  '((2 3  3   8)
     (-8 -4 -12)
        (-5  -5)))

; triangulate: SOE -> TM
; triangulates the given system of equations
(check-expect (triangulate '((1 2))) '((1 2)))
(check-expect (triangulate M) TM)
(check-expect (triangulate M2) TM2)
(define (triangulate M)
  (cond [(empty? (rest M)) M]
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

;;; Does this algorithm terminate for all possible systems of equations?
;;;
;;; No. Imagine you have:
;;;
;;; 1x + 3y  +  9z = 34
;;; 3x + 9y  +  8z = 45
;;; 4x + 12y + 10z = 58
;;;
;;; Its solution is x = 1, y = 2, z = 3
;;;
;;; To triangulate, we start by subtracting the first equation from the rest:
;;;
;;; 1x + 3y +   9z =  34
;;; 0x + 0y + -19z = -57
;;; 0x + 0y + -26z = -78
;;;
;;; But now we are left with:
;;;
;;; 0x + 0y + -19z = -57
;;; 0x + 0y + -26z = -78
;;;
;;; For which no amount of swapping will yield a non-zero coefficient.
;;;
;;; In general, all systems of equations with or without solutions where there are
;;; leading 0 coefficients will cause this to loop forever (or, in this file's
;;; implementation, error out, since `swap-until` will return the original list, and
;;; that'll become a division-by-zero error)
