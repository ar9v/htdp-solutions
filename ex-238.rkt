#lang htdp/isl

;;; Abstract the two functions in figure 89 into a single function. Both consume non-empty
;;; lists of numbers and produce a single number. The left one produces the smallest
;;; number in the list, and the right one the largest.

; Nelon -> Number
; determines the smallest number on l
(define (inf l)
  (cond [(empty? (rest l)) (first l)]
        [else
         (if (< (first l) (inf (rest l)))
             (first l)
             (inf (rest l)))]))

; Nelon -> Number
; determines the largest number on l
(define (sup l)
  (cond [(empty? (rest l)) (first l)]
        [else
         (if (> (first l) (sup (rest l)))
             (first l)
             (sup (rest l)))]))

; pick: (Number Number -> Number) NotEmpty<List<Number>> -> Number
; Given a non empty list of numbers and a comparison function `compare`, picks out the
; number for which `compare` returns it amongst all the numbers in the list.
(define (pick compare nelon)
  (cond [(empty? (rest nelon)) (first nelon)]
        [else
         (if (compare (first nelon) (pick compare (rest nelon)))
             (first nelon)
             (pick compare (rest nelon)))]))

; inf-1: Nelon -> Number
; `inf`, but in terms of `pick`
;
; Commented these out because they are  _s l o w_
; (check-expect (inf-1 l1) (inf l1))
; (check-expect (inf-1 l2) (inf l2))
; (check-expect (sup-1 l1) (sup l1))
; (check-expect (sup-1 l2) (sup l2))
(define (inf-1 nelon)
  (pick < nelon))

; sup-1: Nelon -> Number
; `sup`, but in terms of `pick`
(define (sup-1 nelon)
  (pick > nelon))

;;; Define `inf-1` and `sup-1` in terms of the abstract function. Test them with these
;;; two lists
(define l1
  (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))

(define l2
  (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))

;;; Why are these functions slow on some of the long lists?
;;;
;;; A:
;;; Because the lists are sorted (!) So
;;;
;;; -- For l1:
;;;    -- `inf` makes a recursive call for each element, and since the smallest element is
;;;       the last one, that means it effectively takes O(n^2) time
;;;    -- `sup` is fast, because it immediately finds the largest element
;;;
;;; -- For l2:
;;;    -- `inf` is fast, since it immediately finds the lowest element
;;;    -- `sup` is slow, because it has to go through the entire list to find the largest
;;;       element.

;;; Modify the original functions with the use of `max`, which picks the larger of two
;;; numbers, and `min`, which picks the smaller one. Then abstract again, define `inf-2`
;;; and `sup-2`, and test them with the same inputs again. Why are these versions so much
;;; faster?

; Nelon -> Number
; determines the smallest number on l
(define (inf-min l)
  (cond [(empty? (rest l)) (first l)]
        [else (min (first l) (inf-min (rest l)))]))

; Nelon -> Number
; determines the largest number on l
(define (sup-max l)
  (cond [(empty? (rest l)) (first l)]
        [else (max (first l) (sup (rest l)))]))

; pick.v2: (Number Number -> Number) NotEmpty<List<Number>> -> Number
; Like pick, but uses `compare` only once
(define (pick.v2 compare nelon)
  (cond [(empty? (rest nelon)) (first nelon)]
        [else (compare (first nelon) (pick.v2 compare (rest nelon)))]))

; inf-2: NotEmpty<List<Number>> -> Number
; Like inf-min, but in terms of pick.v2
(check-expect (inf-2 l1) (inf-min l1))
(check-expect (inf-2 l2) (inf-min l2))
(define (inf-2 l) (pick.v2 min l))

; sup-2: NotEmpty<List<Number>> -> Number
; Like sup-max, but in terms of pick.v2
(check-expect (sup-2 l1) (sup-max l1))
(check-expect (sup-2 l2) (sup-max l2))
(define (sup-2 l) (pick.v2 max l))

;;; These versions are much faster because they invariably take O(n) time: they'll
;;; build out a sequence of `max` or `min` calls, which will reduce to the result:
;;;
;;; So, e.g.
;;;
;;; (sup-1 l2) has to do (> 1 (sup-1 (rest l2))) -- which takes a time proportional to the
;;; size of l2 --, answer false and try again with 2 and so on.
;;;
;;; (sup-2 l2) will always do:
;;; (max 1 (max 2 (max ...))) and will reduce down to 25
