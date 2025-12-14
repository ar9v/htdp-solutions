#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; If you run `main`, press the space bar (fire a shot) and wait for a goodly amount of
;;; time, the shot disappears from the canvas. When you shut down the world canvas,
;;; however, the result is a world that still contains this invisible shot.
;;;
;;; Design an alternative `tock` function that doesn't just move shots one pixel per clock
;;; tick but also eliminates those whose coordinates place them above the canvas.
;;;
;;; HINT: You may wish to consider the design of an auxiliary function for the recursive
;;; `cond` clause

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
(check-expect (tock (cons 0 '())) '())
(check-expect (tock (cons 9 (cons 0 (cons 8 '())))) (cons 8 (cons 7 '())))
(define (tock w)
  (cond [(empty? w) '()]
        [(cons? w)
         (if (zero? (first w))
             (tock (rest w))
             (cons (sub1 (first w)) (tock (rest w))))]))

; keyh: ShotWorld KeyEvent -> ShotWorld
; Adds a shot to the world if the player presses the space bar
(check-expect (keyh '() " ") (cons HEIGHT '()))
(check-expect (keyh (cons 9 '()) " ") (cons HEIGHT (cons 9 '())))
(check-expect (keyh '() "a") '())
(check-expect (keyh (cons 9 '()) "a") (cons 9 '()))
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))
