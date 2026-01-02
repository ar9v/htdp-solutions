#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Design a world program that continually moves a one-segment worm and enables a player
;;; to control the movement of the worm with the four cardinal arrow keys. Your program
;;; should use a red disk to render the one-and-only segment of the worm. For each clock
;;; tick, the worm should move a diameter.
;;;
;;; HINTS:
;;; 1. Reread chapter 3.6 to recall how to design world programs. When you define the
;;;    `worm-main` function, use the rate at which the clock ticks as its argument.
;;;
;;; 2. When you develop a data representation for the worm, contemplate the use of two
;;;    different kinds of representations: a physical representation and a logical one.
;;;    The physical representation keeps track of the actual physical position of the worm
;;;    on the canvas; the logical one counts how many (widths of) segments the worm is from
;;;    the left and the top. For which of the two is it easier to change the physical
;;;    appearances  (size of worm segment, size of game box) of the game?

(define WORM-RADIUS 5)
(define WORM-DIAMETER (* WORM-RADIUS 2))
(define WORLD-WIDTH (* WORM-DIAMETER 60))
(define WORLD-HEIGHT WORLD-WIDTH)

(define WORM (circle WORM-RADIUS "solid" "red"))
(define CANVAS (empty-scene WORLD-WIDTH WORLD-HEIGHT))

(define-struct worm [x y direction])
; a Worm is a structure
;  (make-worm Number Number Direction)
;
; interpretation: (make-worm x y d) represents a worm at (x, y) moving LEFT, RIGHT, UP, or
; DOWN
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")

; worm-main: Number -> Worm
; Runs the Worm game. Takes in the clock rate.
(define (worm-main rate)
  (big-bang (make-worm (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2) RIGHT)
            [to-draw render-worm]
            [on-tick update-worm rate]
            [on-key change-worm-direction]))

; render-worm: Worm -> Image
; Places WORM in CANVAS
(check-expect (render-worm (make-worm 10 10 LEFT))
              (place-image WORM 10 10 CANVAS))
(define (render-worm w)
  (place-image WORM (worm-x w) (worm-y w) CANVAS))

; update-worm: Worm -> Worm
; Moves `worm` one diameter in the direction `worm` is heading
(check-expect (update-worm (make-worm 20 20 LEFT))
              (make-worm (- 20 WORM-DIAMETER) 20 LEFT))
(check-expect (update-worm (make-worm 20 20 RIGHT))
              (make-worm (+ 20 WORM-DIAMETER) 20 RIGHT))
(check-expect (update-worm (make-worm 20 20 UP))
              (make-worm 20 (- 20 WORM-DIAMETER) UP))
(check-expect (update-worm (make-worm 20 20 DOWN))
              (make-worm 20 (+ 20 WORM-DIAMETER) DOWN))
(define (update-worm w)
  (cond [(equal? (worm-direction w) LEFT) (worm-up-x w (- (worm-x w) WORM-DIAMETER))]
        [(equal? (worm-direction w) RIGHT) (worm-up-x w (+ (worm-x w) WORM-DIAMETER))]
        [(equal? (worm-direction w) UP) (worm-up-y w (- (worm-y w) WORM-DIAMETER))]
        [(equal? (worm-direction w) DOWN) (worm-up-y w (+ (worm-y w) WORM-DIAMETER))]))


; change-worm-direction: Worm KeyEvent -> Worm
; Changes `worm`'s direction depending on the arrow key pressed; ignores all other keys
(check-expect
 (change-worm-direction (make-worm 50 50 LEFT) "left")
 (make-worm 50 50 LEFT))
(check-expect
 (change-worm-direction (make-worm 50 50 LEFT) "right")
 (make-worm 50 50 RIGHT))
(check-expect
 (change-worm-direction (make-worm 50 50 LEFT) "up")
 (make-worm 50 50 UP))
(check-expect
 (change-worm-direction (make-worm 50 50 LEFT) "down")
 (make-worm 50 50 DOWN))
(check-expect
 (change-worm-direction (make-worm 50 50 LEFT) "a")
 (make-worm 50 50 LEFT))
(define (change-worm-direction w ke)
  (if (or (key=? ke "left")
          (key=? ke "right")
          (key=? ke "up")
          (key=? ke "down"))
      ; This takes advantage that Direction maps to these keys
      (worm-up-direction w ke)
      w))

; worm-up-x: Worm Number -> Worm
; Makes a new worm identical to `w`, save for the new provided `x`
(define (worm-up-x w x)
  (make-worm x (worm-y w) (worm-direction w)))

; worm-up-y: Worm Number -> Worm
; Makes a new worm identical to `w`, save for the new provided `y`
(define (worm-up-y w y)
  (make-worm (worm-x w) y (worm-direction w)))

; worm-up-direction: Worm Direction -> Worm
; Makes a new worm identical to `w`, save for the new provided `direction`
(define (worm-up-direction w direction)
  (make-worm (worm-x w) (worm-y w) direction))
