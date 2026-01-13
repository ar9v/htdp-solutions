#lang htdp/isl

(require 2htdp/image)

;;; Use a `local` expression to organize the functions for drawing a polygon in figure 73.
;;; If a globally defined function is widely useful, do not make it local.

; Image Polygon -> Image
; adds an image of p to MT
(define (render-polygon img p)
  (local (; NELoP -> Image
          ; connects the Posns in `p` in `img`
          (define (connect-dots p)
            (cond
              [(empty? (rest p)) img]
              [else (render-line (connect-dots (rest p))
                                 (first p)
                                 (second p))]))
          (define connected-dots (connect-dots p))
          (define closed-polygon (render-line connected-dots (first p) (last p))))

    closed-polygon))

; Image Posn Posn -> Image
; draws a red line from Posn `p` to Posn `q` into `img`
(define (render-line img p q)
  (scene+line img
              (posn-x p) (posn-y p)
              (posn-x q) (posn-y q)
              "red"))

; [NonEmptyList-of T] -> T
; extracts the last item from `nel`
(define (last nel)
  (cond [(empty? (rest nel)) (first nel)]
        [else (last (rest nel))]))
