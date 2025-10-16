#lang htdp/bsl

(require 2htdp/image)

;; Define the function `image-area`, which counts the number of pixels in a given image.
;; See exercise 6 for ideas.
(define (image-area img)
  (* (image-width img) (image-height img)))
