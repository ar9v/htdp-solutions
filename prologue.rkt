#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; This is used to define a constant. Maybe that means it should be in a module of its own?
(define (image-center-to-h img h)
  (- h (/ (image-height img) 2)))

;; Physical Constants
(define WIDTH 100)
(define HEIGHT 60)
(define ROCK-BED-HEIGHT 10)
(define LANDING-HEIGHT (- HEIGHT ROCK-BED-HEIGHT))
(define LANDING-X-COORD 50)
(define V 3)
(define BACKGROUND-COLOR "blue")

;; Graphical Constants
(define ROCK-BED (rectangle (/ WIDTH 2) ROCK-BED-HEIGHT "solid" "brown"))
(define ROCKET-FILENAME "images/rocket.png")
(define ROCKET (bitmap/file ROCKET-FILENAME))
(define UFO
  (overlay (circle 10 "solid" "green")
           (rectangle 40 4 "solid" "green")))

(define IMAGE UFO)
(define SCENE
  (place-image
   ROCK-BED
   (/ WIDTH 2) (image-center-to-h ROCK-BED HEIGHT)
   (empty-scene WIDTH HEIGHT BACKGROUND-COLOR)))

;; Functions
(define (distance t) (* V t))

(define (picture-of t)
  (cond
    [(<= (distance t) (image-center-to-h IMAGE LANDING-HEIGHT))
     (place-image IMAGE LANDING-X-COORD (distance t) SCENE)]
    [(> (distance t) (image-center-to-h IMAGE LANDING-HEIGHT))
     (place-image IMAGE LANDING-X-COORD (image-center-to-h IMAGE LANDING-HEIGHT) SCENE)]))

(animate picture-of)
