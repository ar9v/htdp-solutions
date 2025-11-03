#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; Design a "virtual cat" world program that continuously moves the cat from left to right.
;; Let's call it `cat-prog` and let's assume it consumes the starting position of the cat.
;; Furthermore, make the cat move three pixels per clock tick. Whenever the cat disappears
;; to the right, it reappears on the left. You may wish to read up on the modulo function.

;;; Physical Constants
(define SCALE 1)
(define WORLD-WIDTH (* SCALE 200))
(define WORLD-HEIGHT (* SCALE (* 2/3 WORLD-WIDTH)))
(define Y-CAT (/ WORLD-HEIGHT 2))
(define DELTA 3)

;;; Graphical Constants
(define cat1 (scale SCALE (bitmap/file "images/cat.png")))
(define CAT-WIDTH (image-width cat1))
(define HALF-CAT (/ CAT-WIDTH 2))
(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT))

;; A Position is a Number
;; interpretation: the x coordinate of the center of the cat

;;; cat-prog: Position -> Position
;;; The main program that moves the cat
(define (cat-prog x)
  (big-bang x
            [to-draw render]
            [on-tick tock]))

;;; render: Position -> Image
;;; Places `cat1` in `BACKGROUND` at (`x` - HALF-CAT, `Y-CAT`)
(check-expect (render -10) (place-image cat1 (- -10 HALF-CAT) Y-CAT BACKGROUND))
(check-expect (render 0) (place-image cat1 (- 0 HALF-CAT) Y-CAT BACKGROUND))
(check-expect (render 10) (place-image cat1 (- 10 HALF-CAT) Y-CAT BACKGROUND))
(define (render x)
  (place-image cat1 (- x HALF-CAT) Y-CAT BACKGROUND))

;;; tock: Position -> Position
;;; moves the cat's `x` position DELTA pixels modulo the WORLD-WIDTH + CAT-WIDTH
(check-expect (tock 0) DELTA)
(check-expect (tock 5) (+ 5 DELTA))
(check-expect (tock WORLD-WIDTH) (+ WORLD-WIDTH DELTA))
(check-expect (tock (+ WORLD-WIDTH CAT-WIDTH)) DELTA)
(define (tock x)
  (modulo (+ x DELTA) (+ WORLD-WIDTH CAT-WIDTH)))
