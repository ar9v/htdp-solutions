#lang htdp/bsl

(require 2htdp/image)

;;; The goal of this exercise is to visualize the result of a
;;; 1968-style European student riot. Here is the rough idea. A small group
;;; of students meets to make paint-filled balloons, enter some lecture hall,
;;; and randomly throws the balloons at the attendees. Your program displays
;;; how the balloons color the seats in the lecture hall.

;;; Use the two functions from exercise 152 to create a rectangle of 8 by 18 squares,
;;; each of which has size 10 by 10. Place it in an `empty-scene` of the same size. This
;;; image is your lecture hall.
;;;
;;; Design `add-balloons`. The function consumes a list of Posn whose coordinates fit into
;;; the dimensions of the lecture hall. It produces an image of the lecture hall with
;;; red dots added as specified by the Posns.

; col: N Image -> Image
; Produces a column of `n` `img`s
(define (col n img)
  (cond [(zero? n) empty-image]
        [(positive? n) (above img (col (sub1 n) img))]))


; row: N Image -> Image
; Produces a row of `n` `img`s
(define (row n img)
  (cond [(zero? n) empty-image]
        [(positive? n) (beside img (row (sub1 n) img))]))

(define SIDE 10)

(define HALL-WIDTH 8)
(define HALL-HEIGHT 18)

(define CELL (square SIDE "outline" "black"))
(define DOT (circle (/ SIDE 3) "solid" "red"))
(define HALL (overlay (col HALL-HEIGHT (row HALL-WIDTH CELL))
                      (empty-scene (* HALL-WIDTH SIDE) (* HALL-HEIGHT SIDE))))

; add-balloons: List-of-Posn -> Image
; Paints HALL with DOTs placed in the coordinates specified in `lop`
(check-expect (add-balloons '()) HALL)
(check-expect (add-balloons (cons (make-posn 2 2) '()))
              (place-image DOT 2 2 HALL))
(check-expect (add-balloons (cons (make-posn 2 2) (cons (make-posn 4 7) '())))
              (place-image DOT
                           4 7
                           (place-image DOT
                                        2 2 HALL)))
(define (add-balloons lop)
  (cond [(empty? lop) HALL]
        [(cons? lop)
         (place-image DOT
                      (posn-x (first lop)) (posn-y (first lop))
                      (add-balloons (rest lop)))]))
