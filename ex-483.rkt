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

; abstract-n-queens:
;   N [N -> Board] [Board QP -> Board] [Board -> [List-of QP]] -> [Maybe [List-of QP]
;
; Returns a list of positions that solve the n-queens problem, if possible
;
; Constraints: `add-queen` and `find-open-spots` must be compatible with whatever
; representation of a board `board0` creates.
(check-satisfied (abstract-n-queens
                  4
                  available/board0
                  available/add-queen
                  available/find-open-spots)
                 (n-queens-solution? 4))
(check-satisfied (abstract-n-queens
                  4
                  taken/board0
                  taken/add-queen
                  (taken/make-find-open-spot-checker 4))
                 (n-queens-solution? 4))
(check-satisfied (abstract-n-queens
                  5
                  available/board0
                  available/add-queen
                  available/find-open-spots)
                 (n-queens-solution? 5))
(check-satisfied (abstract-n-queens
                  5
                  taken/board0
                  taken/add-queen
                  (taken/make-find-open-spot-checker 5))
                 (n-queens-solution? 5))
(define (abstract-n-queens n board0 add-queen find-open-spots)
  (local [(define (abstract-place-queens a-board n)
            (cond [(zero? n) '()]
                  [else
                   (for/or [(p (find-open-spots a-board))]
                     (local [(define candidate
                               (abstract-place-queens (add-queen a-board p) (sub1 n)))]
                       (if (list? candidate) (cons p candidate) #false)))]))]
    (abstract-place-queens (board0 n) n)))


(check-satisfied (n-queens 4) (n-queens-solution? 4))
(check-satisfied (n-queens 5) (n-queens-solution? 5))
(define (n-queens n)
  (abstract-n-queens n available/board0 available/add-queen available/find-open-spots))

; available/board0: N -> Board
; creates the initial n by n board, represented as a list of spots that are still
; available
(define (available/board0 n)
  (for*/list [(i n) (j n)] (make-posn i j)))

; available/add-queen: Board QP -> Board
; places a queen at `qp` on `a-board`
(define (available/add-queen a-board qp)
  (filter (λ (p) (not (threatening? qp p))) (remove qp a-board)))

; available/find-open-spots: Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (available/find-open-spots a-board)
  a-board)

; taken/board0: N -> Board
; creates the initial n by n board, represented as a list of taken spots
(define (taken/board0 _n)
  '())

; taken/add-queen: Board QP -> Board
; places a queen at `qp` on `a-board`
(define (taken/add-queen a-board qp)
  (cons qp a-board))

; taken/make-find-open-spot-checker: N -> [Board -> [List-of QP]]
; Given `n` produces a function that uses `taken/find-open-spots` to
; produce a list of candidates for available squares.
;
; NOTE: The exercise asks to consider the possibility of thinking "a Board contains the
;       list of positions where a queen has been placed". It _doesn't_ say it's necessarily
;       a [List-of QP], it may as well be a struct. I thought it'd be interesting to try
;       to take it as literally as possible. Calling `abstract-n-queens` with this is
;       clunky (and liable to errors, if we mix up the numbers), but the contract _is_
;       respected.
(define (taken/make-find-open-spot-checker n)
  (λ (a-board) (taken/find-open-spots a-board n)))

; taken/find-open-spots: Board -> [List-of QP]
; finds spots where it is still safe to place a queen
;
; NOTE: While `universe` is basically `available/board0`, the idea here is that both
;       representations remain separated.
(define (taken/find-open-spots a-board n)
  (local [(define universe (for*/list [(i n) (j n)] (make-posn i j)))]
    (foldr (λ (queen posns) (filter (λ (p) (not (threatening? queen p))) posns))
           universe
           a-board)))

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
