#lang htdp/bsl

(require 2htdp/image)

;;; Design two functions: `col` and `row`.
;;;
;;; The function `col` consumes a natural number `n` and an image `img`. It produces a
;;; column -- a vertical arrangement -- of `n` copies of `img`.
;;;
;;; The function `row` consumes a natural number `n` and an image `img`. It produces a
;;; row -- a horizontal arrangement -- of `n` copies of `img`.

(define SQR (square 3 "solid" "black"))

; col: N Image -> Image
; Produces a column of `n` `img`s
(check-expect (col 0 SQR) empty-image)
(check-expect (col 2 SQR) (above SQR (above SQR empty-image)))
(define (col n img)
  (cond [(zero? n) empty-image]
        [(positive? n) (above img (col (sub1 n) img))]))


; row: N Image -> Image
; Produces a row of `n` `img`s
(check-expect (row 0 SQR) empty-image)
(check-expect (row 2 SQR) (beside SQR (beside SQR empty-image)))
(define (row n img)
  (cond [(zero? n) empty-image]
        [(positive? n) (beside img (row (sub1 n) img))]))
