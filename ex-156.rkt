#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Equip the program in figure 61 with tests and make sure it passes those.

(define HEIGHT 80) ; distances in terms of pixels
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))

; graphical constants
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))

; A ShotWorld is a List-of-numbers
;
; interpretation: each number on such a list represents the y-coordinate of a shot

; main: ShotWorld -> ShotWorld
(define (main w)
  (big-bang w
            [to-draw to-image]
            [on-tick tock]
            [on-key keyh]))

; to-image: ShotWorld -> Image
; Adds the image of a shot for each `y` on `w` at (MID, y) to the BACKGROUND
(check-expect (to-image '()) BACKGROUND)
(check-expect (to-image (cons 9 '()))
              (place-image SHOT
                           XSHOTS 9 BACKGROUND))
(check-expect (to-image (cons 18 (cons 9 '())))
              (place-image SHOT
                           XSHOTS 18
                           (place-image SHOT
                                        XSHOTS 9
                                        BACKGROUND)))
(define (to-image w)
  (cond [(empty? w) BACKGROUND]
        [(cons? w)
         (place-image SHOT XSHOTS (first w)
                      (to-image (rest w)))]))

; tock: ShotWorld -> ShotWorld
; Moves each shot on `w` up by one pixel
(check-expect (tock '()) '())
(check-expect (tock (cons 9 '())) (cons 8 '()))
(check-expect (tock (cons 18 (cons 9 '()))) (cons 17 (cons 8 '())))
(define (tock w)
  (cond [(empty? w) '()]
        [(cons? w)
         (cons (sub1 (first w))
               (tock (rest w)))]))

; keyh: ShotWorld KeyEvent -> ShotWorld
; Adds a shot to the world if the player presses the space bar
(check-expect (keyh '() " ") (cons HEIGHT '()))
(check-expect (keyh (cons 9 '()) " ") (cons HEIGHT (cons 9 '())))
(check-expect (keyh '() "a") '())
(check-expect (keyh (cons 9 '()) "a") (cons 9 '()))
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))

(main '())
