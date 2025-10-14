#lang htdp/bsl

(require 2htdp/image)

;; Add the following line to the definitions area
(define cat (bitmap/file "images/cat.png"))

;; Create a conditional expression that computes whether the image is tall or
;; wide. An image should be labeled "tall" if its height is larger than or
;; equal to its width; otherwise it is "wide".
(string=?
 (if (< (image-width cat) (image-height cat)) "tall" "wide")
 "tall")

;; Now try the following modification. Create an expression that computes whether a picture
;; is "tall", "wide", or "square".
(define (aspect-ratio image)
  (if (< (image-width image) (image-height image))
    "tall"
    (if (= (image-width image) (image-height image)) "square" "wide")))

(define (aspect-ratio-cond image)
  (cond [(< (image-width image) (image-height image)) "tall"]
        [(= (image-width image) (image-height image)) "square"]
        [else "wide"]))

;; Some tests
(string=?
 "square"
 (aspect-ratio (square 5 "solid" "red")))

(string=?
 "square"
 (aspect-ratio (circle 8 "solid" "blue")))

(string=?
 "wide"
 (aspect-ratio (rectangle 10 5 "solid" "yellow")))

(string=?
 "tall"
 (aspect-ratio cat))

(string=?
 "square"
 (aspect-ratio-cond (square 5 "solid" "red")))

(string=?
 "square"
 (aspect-ratio-cond (circle 8 "solid" "blue")))

(string=?
 "wide"
 (aspect-ratio-cond (rectangle 10 5 "solid" "yellow")))

(string=?
 "tall"
 (aspect-ratio-cond cat))
