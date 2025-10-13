#lang htdp/bsl

(require 2htdp/image)

;; Add the following line to the definitions area

(define CAT (bitmap/file "images/cat.png")) ; The text has the image, of course

;; Create an expression that counts the number of pixels in the image
(* (image-width CAT) (image-height CAT))
