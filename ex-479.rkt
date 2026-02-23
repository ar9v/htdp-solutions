#lang htdp/isl+

;;; Design the `threatening?` function. It consumes two QP's and determines whether queens
;;; placed on the two respective squares would threaten each other.

; A QP is a structure:
;   (make-posn CI CI)
;
; A CI is an N in [0, QUEENS).
; (define QUEENS 8)
;
; interpretation: (make-posn r c) denotes the square at the r-th row and c-th column

; threatening?: QP QP -> Boolean
; #true if queens placed at qp1 and qp2 would threaten each other.
(check-expect (threatening? (make-posn 0 1) (make-posn 0 3)) #true)
(check-expect (threatening? (make-posn 1 3) (make-posn 5 3)) #true)
(check-expect (threatening? (make-posn 1 2) (make-posn 0 3)) #true)
(check-expect (threatening? (make-posn 3 4) (make-posn 5 6)) #true)
(check-expect (threatening? (make-posn 0 2) (make-posn 1 4)) #false)
(define (threatening? qp1 qp2)
  (local [(define (same-row? p1 p2) (= (posn-x p1) (posn-x p2)))
          (define (same-col? p1 p2) (= (posn-y p1) (posn-y p2)))
          (define (same-diagonal? p1 p2)
            (or (= (+ (posn-x p1) (posn-y p1)) (+ (posn-x p2) (posn-y p2)))
                (= (- (posn-x p1) (posn-y p1)) (- (posn-x p2) (posn-y p2)))))]
    (or (same-row? qp1 qp2)
        (same-col? qp1 qp2)
        (same-diagonal? qp1 qp2))))
