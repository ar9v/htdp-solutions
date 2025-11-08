#lang htdp/bsl

(require 2htdp/image)

;;; Take another look at `show`. It contains three instances of an expression with the
;;; approximate shape:
;;;
;;; (place-image ROCKET 10 (- ... CENTER) BACKGROUND)

;;; This expression appears three times in the function: twice to draw a resting rocket
;;; and once to draw a flying rocket. Define an auxiliary function that performs this
;;; work and thus shorten `show`. Why is this a good idea? You may want to reread the
;;; Prologue.

;;; This is a good idea because it makes the program easier to change!

;; Constants
(define HEIGHT 300) ; distances in pixels
(define WIDTH 100)
(define YDELTA 3)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define ROCKET (bitmap/file "images/rocket.png"))

(define CENTER (/ (image-height ROCKET) 2))

; an LRCD (for launching rocket countdown) is one of:
; -- "resting"
; -- a Number between -3 and -1
; -- a NonnegativeNumber
;
; interpretation: a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)

; show: LRCD -> Image
; render the state as a resting or flying rocket
(check-expect (show "resting") (draw-rocket (- HEIGHT CENTER)))

(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              10 (* 3/4 WIDTH)
              (draw-rocket (- HEIGHT CENTER))))

(check-expect
 (show 53)
 (draw-rocket (- 53 CENTER)))

(check-expect
 (show HEIGHT)
 (draw-rocket (- HEIGHT CENTER)))

;;; This renders the image when the rocket's left our line of sight.
;;; The book states that writing this test is a "simple but revealing exercise". Here's
;;; what stuck with me:
;;;
;;; Maybe you'd think that you shouldn't be able to produce an image with a negative
;;; coordinate, because that would possibly fall out of LRCD's interval. But `show`'s
;;; signature is LRCD -> Image; nothing about the ensuing Image is breaking the contract of
;;; the type(!)
(check-expect
 (show 0)
 (draw-rocket (- 0 CENTER)))

(define (show x)
  (cond [(string? x)
         (draw-rocket (- HEIGHT CENTER))]
        [(<= -3 x -1)
         (place-image (text (number->string x) 20 "red")
                      10 (* 3/4 WIDTH)
                      (draw-rocket (- HEIGHT CENTER)))]
        [(>= x 0)
         (draw-rocket (- x CENTER))]))

;;; draw-rocket: Number -> Image
;;; Renders ROCKET at position (10, `y`) on BACKGROUND
(check-expect (draw-rocket 0) (place-image ROCKET 10 0 BACKGROUND))
(check-expect (draw-rocket 50) (place-image ROCKET 10 50 BACKGROUND))
(check-expect (draw-rocket HEIGHT) (place-image ROCKET 10 HEIGHT BACKGROUND))
(define (draw-rocket y)
  (place-image ROCKET 10 y BACKGROUND))
