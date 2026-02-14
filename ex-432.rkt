#lang htdp/isl+

;;; Exercise 219 introduces the function `food-create`, which consumes a Posn and produces
;;; a randomly chosen Posn that is guaranteed to be distinct from the given one. First
;;; reformulate the two functions as a single definition, using `local`; then justify the
;;; design of `food-create`.

(define MAX 20)

; food-create: Posn -> Posn
; Creates a random Posn with coordinates ((random MAX), (random MAX)) that is NOT `p`
(define (food-create p)
  (local [(define (food-check-create p candidate)
            (if (equal? p candidate) (food-create p) candidate))]
    (food-check-create p (make-posn (random MAX) (random MAX)))))

;;; In `food-create`, even though it is not exposed to the caller, the trivial case is
;;; generating a random Posn that is different from the given one. That case is solved
;;; simply by returning the generated Posn.
;;;
;;; The non-trivial case is having to generate a new, distinct Posn. That problem, however,
;;; is solved in the same way the original was; we do not need to combine results. Unless
;;; MAX == 1 and the passed in Posn has the same constraints (i.e. it is (0, 0)) then the
;;; algorithm is guaranteed is to terminate.
