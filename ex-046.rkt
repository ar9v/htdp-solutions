#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Improve the cat animation with a slightly different image:
;; (define cat2 (bitmap/file "images/cat2.png"))

;;; Adjust the rendering function from exercise 45 so that it uses one cat image or the
;;; other based on whether the x-coordinate is odd. Read up on `odd?` in the HelpDesk,
;;; and use a `cond` expression to select cat images.
;;; Physical Constants
(define SCALE 1)
(define WORLD-WIDTH (* SCALE 200))
(define WORLD-HEIGHT (* SCALE (* 2/3 WORLD-WIDTH)))
(define Y-CAT (/ WORLD-HEIGHT 2))
(define DELTA 3)

;;; Graphical Constants
(define cat1 (scale SCALE (bitmap/file "images/cat.png")))
(define cat2 (scale SCALE (bitmap/file "images/cat2.png")))
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
;;;
;;; Notice we changed the tests: `check-expect` will pass regardless of the cat image we
;;; use if the cat is out of the frame since the resulting image is, technically, the
;;; same (!!)
(check-expect (render 0) (place-image cat2 (- 0 HALF-CAT) Y-CAT BACKGROUND))
(check-expect (render (sub1 (/ WORLD-WIDTH 2)))
              (place-image cat1 (- (sub1 (/ WORLD-WIDTH 2)) HALF-CAT) Y-CAT BACKGROUND))
(check-expect (render (/ WORLD-WIDTH 2))
              (place-image cat2 (- (/ WORLD-WIDTH 2) HALF-CAT) Y-CAT BACKGROUND))
(define (render x)
  (place-image
   (cond [(odd? x) cat1]
         [else cat2])
   (- x HALF-CAT)
   Y-CAT
   BACKGROUND))

;;; tock: Position -> Position
;;; moves the cat's `x` position DELTA pixels modulo the WORLD-WIDTH + CAT-WIDTH
(check-expect (tock 0) DELTA)
(check-expect (tock 5) (+ 5 DELTA))
(check-expect (tock WORLD-WIDTH) (+ WORLD-WIDTH DELTA))
(check-expect (tock (+ WORLD-WIDTH CAT-WIDTH)) DELTA)
(define (tock x)
  (modulo (+ x DELTA) (+ WORLD-WIDTH CAT-WIDTH)))
