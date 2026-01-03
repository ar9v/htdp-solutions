#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Modify your program from exercise 215 so that it stops if the worm has reached the
;;; walls of the world. When the program stops because of this condition, it should render
;;; the final scene with the text "worm hit border" in the lower left of the world scene.
;;;
;;; HINT:
;;; You can use the `stop-when` clause in `big-bang` to render the last world in a special
;;; way

(define WORM-RADIUS 5)
(define WORM-DIAMETER (* WORM-RADIUS 2))
(define WORLD-WIDTH (* WORM-DIAMETER 30))
(define WORLD-HEIGHT WORLD-WIDTH)

(define GAME-OVER-TEXT (text "worm hit border" 16 "black"))
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
            [on-key change-worm-direction]
            [stop-when worm-hit-wall? render-last-world]))

; render-worm: Worm -> Image
; Places WORM in CANVAS
(check-expect (render-worm (make-worm 10 10 LEFT))
              (place-image WORM 10 10 CANVAS))
(define (render-worm w)
  (place-image WORM (worm-x w) (worm-y w) CANVAS))

; render-last-world: Worm -> Image
; Renders the last state with an overlayed GAME-OVER-TEXT
(check-expect (render-last-world (make-worm 2 10 LEFT))
              (overlay/align
               "left" "bottom"
               GAME-OVER-TEXT
               (render-worm (make-worm 2 10 LEFT))))
(define (render-last-world w)
  (overlay/align
   "left" "bottom"
   GAME-OVER-TEXT
   (render-worm w)))

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

; worm-hit-wall?: Worm -> Boolean
; Determines if the Worm `w` is in a position where it has hit a wall
(check-expect
 (worm-hit-wall? (make-worm (- WORM-RADIUS 1) WORM-DIAMETER LEFT)) #true)
(check-expect
 (worm-hit-wall? (make-worm WORM-RADIUS WORM-DIAMETER LEFT)) #true)
(check-expect
 (worm-hit-wall? (make-worm (+ 1 WORM-RADIUS) WORM-DIAMETER LEFT)) #false)
(check-expect
 (worm-hit-wall? (make-worm (- WORLD-WIDTH WORM-RADIUS 1) WORM-DIAMETER LEFT)) #false)
(check-expect
 (worm-hit-wall? (make-worm (- WORLD-WIDTH WORM-RADIUS) WORM-DIAMETER LEFT)) #true)
(check-expect
 (worm-hit-wall? (make-worm (- WORLD-WIDTH (- WORM-RADIUS 1)) WORM-DIAMETER LEFT)) #true)

(check-expect
 (worm-hit-wall? (make-worm WORM-DIAMETER (- WORM-RADIUS 1) LEFT)) #true)
(check-expect
 (worm-hit-wall? (make-worm WORM-DIAMETER WORM-RADIUS LEFT)) #true)
(check-expect
 (worm-hit-wall? (make-worm WORM-DIAMETER (+ 1 WORM-RADIUS) LEFT)) #false)
(check-expect
 (worm-hit-wall? (make-worm WORM-DIAMETER (- WORLD-HEIGHT WORM-RADIUS 1) LEFT)) #false)
(check-expect
 (worm-hit-wall? (make-worm WORM-DIAMETER (- WORLD-HEIGHT WORM-RADIUS) LEFT)) #true)
(check-expect
 (worm-hit-wall? (make-worm WORM-DIAMETER (- WORLD-HEIGHT (- WORM-RADIUS 1)) LEFT)) #true)
(define (worm-hit-wall? w)
  (or (<= (- (worm-x w) WORM-RADIUS) 0)
      (<= WORLD-WIDTH (+ (worm-x w) WORM-RADIUS))
      (<= WORLD-HEIGHT (+ (worm-y w) WORM-RADIUS))
      (<= (- (worm-y w) WORM-RADIUS) 0)))

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
