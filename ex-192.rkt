#lang htdp/bsl+

(require 2htdp/image)

;;; Argue why it is acceptable to use `last` on Polygons.

;;; A:
;;;
;;; Because all Polygons are non empty lists of points (NELoPs)


;;; Also argue why you may adapt the template for `connect-dots` to `last`
;;;
;;; (define (last p)
;;;   (cond [(empty (rest p)) (... (first p) ...)]
;;;         [else (... (first p) ... (last (rest p) ...))]))
;;;
;;; A:
;;;
;;; Because `connect-dots` also processes a non empty list of points.


;;; Finally, develop examples for `last`, turn them into tests, and ensure that the
;;; definition for `last` in figure 73 works on your examples.

(define triangle-p
  (list (make-posn 20 10)
        (make-posn 20 20)
        (make-posn 30 20)))

(define square-p
  (list (make-posn 10 10)
        (make-posn 20 10)
        (make-posn 20 20)
        (make-posn 10 20)))

(define pentagon-p
  (list (make-posn 10 10)
        (make-posn 5 15)
        (make-posn 15 20)
        (make-posn 25 15)
        (make-posn 20 10)))

; last: Polygon -> Posn
; extracts the last item from p
(check-expect (last triangle-p) (make-posn 30 20))
(check-expect (last square-p) (make-posn 10 20))
(check-expect (last pentagon-p) (make-posn 20 10))
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))
