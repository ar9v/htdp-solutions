#lang htdp/isl+

(require 2htdp/image)

;;; Design `render-queens`. The function consumes a natural number `n`, a list of QPs, and
;;; an Image. It produces an image of an n by n chess board with the given image placed
;;; according to the given QPs.
;;;
;;; You may wish to look for an image for a chess queen on-line or create a simplistic
;;; one with the available image functions.

(define QUEEN (bitmap/file "images/queen.png"))

(define SQUARE-SIZE (+ (image-width QUEEN) 20))
(define SQUARE (overlay (square (- SQUARE-SIZE 2) 'solid 'white)
                        (square SQUARE-SIZE 'solid 'black)))

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
