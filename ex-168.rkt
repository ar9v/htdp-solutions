#lang htdp/bsl

;;; Design the function `translate`. It consumes and produces lists of Posns. For each
;;; (make-posn x y) in the former, the latter contains (make-posn x (+ y 1)). We borrow
;;; the word "translate" from geometry, where the movement of a point by a constant
;;; distance along a straight line is called a `translation`

(define empty-lop '())
(define lop-1
  (cons (make-posn 1 2)
        (cons (make-posn 3 4)
              empty)))

; translate: List-of-Posns -> List-of-Posns
; Translates `lop` by modifying all y-coordinates in it
(check-expect (translate empty-lop) empty-lop)
(check-expect (translate lop-1)
              (cons (make-posn 1 3) (cons (make-posn 3 5) empty)))
(define (translate lop)
  (cond [(empty? lop) '()]
        [(cons? lop)
         (cons (posn-y-add1 (first lop))
               (translate (rest lop)))]))

; posn-y-add1: Posn -> Posn
; Returns a new Posn, which is the result of adding 1 to the given `pos`
(check-expect (posn-y-add1 (make-posn 1 2)) (make-posn 1 3))
(define (posn-y-add1 pos)
  (make-posn (posn-x pos) (add1 (posn-y pos))))
