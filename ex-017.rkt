#lang htdp/bsl

(require 2htdp/image)

(define cat (bitmap/file "images/cat.png"))

;; Define the function `image-classify`, which consumes an image and conditionally produces
;; "tall" if the image is taller than wide, "wid" if it is wider than tall, or "square"
;; if its width and height are the same. See exercise 8 for ideas.

;; Woops, back in ex-008.rkt I'd already done this, w/ a different name.
(define (image-classify image)
  (cond [(< (image-width image) (image-height image)) "tall"]
        [(= (image-width image) (image-height image)) "square"]
        [else "wide"]))

(string=?
 "square"
 (image-classify (square 5 "solid" "red")))

(string=?
 "square"
 (image-classify (circle 8 "solid" "blue")))

(string=?
 "wide"
 (image-classify (rectangle 10 5 "solid" "yellow")))

(string=?
 "tall"
 (image-classify cat))
