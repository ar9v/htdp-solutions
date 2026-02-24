#lang htdp/isl+

(require 2htdp/abstraction)
(require 2htdp/image)

;;; Develop a data definition for Board and design the three
;;; functions specified in exercise 482. Consider the following ideas:
;;;
;;; -- a Board collects those positions where a queen can still be placed;
;;; -- a Board contains the list of positions where a queen has been placed;
;;; -- a Board is a grid of n by n squares, each possibly occupied by a queen.
;;;    Use a structure with three fields to represent a square: one for x, one
;;;    for y, and a third one saying whether the square is threatened.
;;;
;;; Use one of the above ideas to solve this exercise.
;;;
;;; CHALLENGE: Use all three ideas to come up with three different data representations
;;; of a Board. Abstract your solution to exercise 482 and confirm that it works with
;;; any of your data representations of a Board.

; A Board is a [List-of QP]
;
; Interpretation: A Board contains all available places

(define QUEEN (bitmap/file "images/queen.png"))

(define SQUARE-SIZE (+ (image-width QUEEN) 20))
(define SQUARE (overlay (square (- SQUARE-SIZE 2) 'solid 'white)
                        (square SQUARE-SIZE 'solid 'black)))

; n-queens-solution?: N -> [[List-of QP] -> Boolean]
; Returns a function that checks whether its argument is a valid solution to the n-queens
; problem, for the value `n`.
(define (n-queens-solution? n)
  (λ (qps)
    (and (list? qps)
         (= (length qps) n)
         (for*/and [(qp1 qps) (qp2 qps)]
           (or (equal? qp1 qp2) (not (threatening? qp1 qp2)))))))

(check-satisfied (n-queens 4) (n-queens-solution? 4))
(check-satisfied (n-queens 5) (n-queens-solution? 5))
(define (n-queens n)
  (place-queens (board0 n) n))

; place-queens: Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise returns #false
(define (place-queens a-board n)
  (cond [(zero? n) '()]
        [else (for/or [(p (find-open-spots a-board))]
                (local [(define candidate (place-queens (add-queen a-board p) (sub1 n)))]
                  (if (list? candidate) (cons p candidate) #false)))]))

; board0: N -> Board
; creates the initial n by n board
(define (board0 n)
  (for*/list [(i n) (j n)] (make-posn i j)))

; add-queen: Board QP -> Board
; places a queen at `qp` on `a-board`
(define (add-queen a-board qp)
  (filter (λ (p) (not (threatening? qp p))) (remove qp a-board)))

; find-open-spots: Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots a-board)
  a-board)

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

; render-queens: N [List-of QP] Image -> Image
; Renders `img` on an `n` by `n` board, at positions `qps`
(check-expect
 (render-queens 3 (list (make-posn 0 1) (make-posn 1 1)) QUEEN)
 (place-image/align
  QUEEN
  (+ (* 1 SQUARE-SIZE) (/ SQUARE-SIZE 2))
  (+ (* 0 SQUARE-SIZE) (/ SQUARE-SIZE 2))
  'center 'center
  (place-image/align
   QUEEN
   (+ (* 1 SQUARE-SIZE) (/ SQUARE-SIZE 2))
   (+ (* 1 SQUARE-SIZE) (/ SQUARE-SIZE 2))
   'center 'center
   (foldr above empty-image (make-list 3 (render-row 3 SQUARE))))))
(define (render-queens n qps img)
  (local [(define (render-queen qp bg)
            (local [(define (calculate-coord logical-coord)
                      (+ (* logical-coord SQUARE-SIZE) offset))
                    (define offset (/ SQUARE-SIZE 2))]
              (place-image/align img
                                 (calculate-coord (posn-y qp))
                                 (calculate-coord (posn-x qp))
                                 'center 'center
                                 bg)))
          (define (render-board n)
            (foldr above empty-image (make-list n (render-row n SQUARE))))]
    (foldr (λ (qp bg) (render-queen qp bg)) (render-board n) qps)))

; render-row: N -> Image
; Produces an image made up of `n` `img`s, placed horizontally
(check-expect (render-row 3 SQUARE)
              (beside SQUARE (beside SQUARE (beside SQUARE empty-image))))
(define (render-row n img)
  (foldr beside empty-image (make-list n img)))
