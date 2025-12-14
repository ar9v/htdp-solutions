#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Experiment to determine whether the arbitrary decisions considering constants are
;;; easy to change. For example, determine whether changing a single constant definition
;;; achieves the desired outcome:
;;;
;;; -- change the height of the canvas to 220 pixels
;;; -- change the width of the canvas to 30 pixels
;;; -- change the "x" location of the line of shots to
;;;    "somewhere to the left of the middle"
;;; -- change the background to a green rectangle; and
;;; --  change the rendering of shots to a red elongated rectangle

;;; A: Overall, these changes are easy to make, and the tests pass. I do reckon that
;;;    the changes are made in a way that are still fairly "benign". E.g. if I changed
;;;    the shot to some arbitrary size, there's no guarantee it'd fit in the background.
;;;
;;;    So, there's no single point of control. There's a trade-off there: these things are
;;;    easier to come up with and write on the spot (i.e. we don't need to think of how
;;;    everything is related upfront). But we do need to be careful not to make changes
;;;    that'll work with the rest of the constants.

(define HEIGHT 220) ; <- CHANGED
(define WIDTH 30)   ; <- CHANGED
(define XSHOTS (/ WIDTH 4)) ; <- CHANGED

; graphical constants
(define BACKGROUND (empty-scene WIDTH HEIGHT "green")) ; <- CHANGED
(define SHOT (rectangle 2 10 "solid" "red")) ; <- CHANGED

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
