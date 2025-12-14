#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Turn the solution of exercise 153 into a world program. Its main function, dubbed
;;; `riot`, consumes how many balloons the students want to throw; its visualization shows
;;; one balloon dropping after another at a rate of one per second. The function produces
;;; a list of Posns where the balloons hit.

;;; HINT (1) Here is one possible data representation:
(define-struct pair [balloon# lob])
; A Pair is a structure (make-pair N List-of-posns)
; A List-of-posns is one of:
; -- '()
; -- (cons Posn List-of-posns)
;
; interpretation: (make-pair n lob) means `n` balloons must yet be thrown and added to
; `lob`

;;; HINT (2) A `big-bang` expression is really just an expression. It is legitimate to
;;; nest it within another expression.

;;; HINT (3) Recall that `random` creates random numbers.

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

(define RATE 1)
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

; riot: N -> List-of-posns
(define (riot n)
  (pair-lob
   (big-bang (make-pair n '())
             [to-draw render]
             [on-tick add-balloon RATE]
             [stop-when no-more-balloons? render])))

; render: Pair -> Image
; Renders the `p`s balloons onto the background
(check-expect (render (make-pair 0 '())) HALL)
(check-expect (render (make-pair 0 (cons (make-posn 1 1) '())))
              (add-balloons (cons (make-posn 1 1) '())))
(check-expect (render (make-pair 3 (cons (make-posn 1 1) '())))
              (add-balloons (cons (make-posn 1 1) '())))
(define (render p)
  (add-balloons (pair-lob p)))

; add-balloon: Pair -> Pair
; Generates a new Posn to add to Pair's list of balloons
(check-random (add-balloon (make-pair 1 '()))
              (make-pair 0
                         (cons (make-posn (random (* HALL-WIDTH SIDE))
                                          (random (* HALL-HEIGHT SIDE)))
                               '())))
(define (add-balloon p)
  (make-pair
   (sub1 (pair-balloon# p))
   (cons (make-posn (random (* HALL-WIDTH SIDE))
                    (random (* HALL-HEIGHT SIDE)))
         (pair-lob p))))

; no-more-balloons?: Pair -> Boolean
; True if Pair's `n` is 0
(check-expect (no-more-balloons? (make-pair 1 (cons (make-posn 1 2) '()))) #false)
(check-expect (no-more-balloons? (make-pair 0 (cons (make-posn 1 2) '()))) #true)
(define (no-more-balloons? p)
  (zero? (pair-balloon# p)))
