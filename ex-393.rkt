#lang htdp/isl+

;;; Figure 62 presents two data definitions for finite sets. Design the `union` function
;;; for the representation of finite sets of your choice. It consumes two sets and
;;; produces one that contains the elements of both.

;;; Design `intersect` for the same set representation. It consumes two sets and produces
;;; the set of exactly those elements that occur in both.

; A [Set-of Number] is one of
; -- empty
; -- (cons Number [Set-of Number])
;
; Constraint If s is a [Set-of Number], no number occurs twice in s

; union: [Set-of N] [Set-of N] -> [Set-of N]
; Given `s1` and `s2`, produces a new set that has all elements of `s1` and
; `s2` (with no duplicates)
(check-expect (union '() '()) '())
(check-expect (union '() '(1 2)) '(1 2))
(check-expect (union '(1 2) '()) '(1 2))
(check-expect (union '(1 2) '(3 4)) '(1 2 3 4))
(check-expect (union '(1 2 3) '(3 4)) '(1 2 3 4))
(check-expect (union '(2 1) '(1 3 4 5 2)) '(1 2 3 4 5))
(define (union s1 s2)
  (local [(define (union-sorted s1 s2)
            (cond [(empty? s1) s2]
                  [(empty? s2) s1]
                  [else (local [(define e1 (first s1))
                                (define e2 (first s2))]
                          (cons e1 (union-sorted (rest s1) (if (= e1 e2) (rest s2) s2))))]))]
    (union-sorted (sort s1 <) (sort s2 <))))

; intersect: [Set-of N] [Set-of N] -> [Set-of N]
; Given `s1` and `s2`, produces the set that contains only the numbers that appear in both
; `s1` and `s2`
(check-expect (intersect '() '()) '())
(check-expect (intersect '() '(1 2)) '())
(check-expect (intersect '(1 2) '()) '())
(check-expect (intersect '(1 3 2) '(4 3 2)) '(2 3))
(check-expect (intersect '(1 2 3) '(4 5 6)) '())
(define (intersect s1 s2)
  (local [(define (intersect-sorted s1 s2)
            (cond [(or (empty? s1) (empty? s2)) '()]
                  [else
                   (local [(define e1 (first s1))
                           (define e2 (first s2))]
                     (cond [(< e1 e2) (intersect-sorted (rest s1) s2)]
                           [(> e1 e2) (intersect-sorted s1 (rest s2))]
                           [else (cons e1 (intersect-sorted (rest s1) (rest s2)))]))]))]
    (intersect-sorted (sort s1 <) (sort s2 <))))
