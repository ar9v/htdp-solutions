#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Design `my-animate`. Recall that the `animate` function consumes the representation
;;; of a *stream* of images, one per natural number. Since streams are infinitely long,
;;; ordinary compound data cannot represent them. Instead, we use functions:

; An ImageStream is a function:
;   [ N -> Image]
;
; interpretation: a stream `s` denotes a series of images

;;; Here is a data example

(define ROCKET (bitmap/file "images/rocket.png"))

; ImageStream
(define (create-rocket-scene height)
  (place-image ROCKET 50 height (empty-scene 60 60)))

;;; You may recognize this as one of the first pieces of code in the Prologue.
;;;
;;; The job of `(my-animate s n)` is to show the images `(s 0)`, `(s 1)`, and so on at
;;; a rate of 30 images per second up to `n` images total. Its result is the number of
;;; clock ticks passed since launched.

(define RATE 1/30)

; my-animate: ImageStream N -> N
; Shows `n` frames of stream `s`
(check-expect (my-animate create-rocket-scene 30) 30)
(define (my-animate s n)
  (big-bang 0
            [to-draw s]
            [on-tick add1 RATE]
            [stop-when (λ (curr) (= curr n))]))
