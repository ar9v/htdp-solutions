#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Use the lessons learned from the preceding two sections and design the game extension
;;; slowly, adding one feature of the game after another. Always use the design recipe and
;;; rely on the guidelines for auxiliary functions. If you like the game, add other features:
;;;
;;; - show a running text
;;; - equip the UFO with charges that can eliminate the tank
;;; - create an entire fleet of attacking UFOs
;;;
;;; and above all, use your imagination.

; beside/list: [Image] -> Image
; Like 2htdp/image's `beside`, but takes in a list
(check-expect (beside/list '()) empty-image)
(check-expect (beside/list (list TANK CANNON TANK))
              (beside TANK (beside CANNON (beside TANK empty-image))))
(define (beside/list imgs)
  (cond [(empty? imgs) empty-image]
        [(cons? imgs) (beside (first imgs) (beside/list (rest imgs)))]))


(define TANK-WIDTH 50)
(define TANK-HEIGHT (* 1/3 TANK-WIDTH))
(define CANNON-WIDTH (* 1/6 TANK-WIDTH))
(define CANNON-HEIGHT TANK-HEIGHT)
(define WHEEL-AMOUNT 5)
(define WHEEL-RADIUS (/ TANK-WIDTH (* WHEEL-AMOUNT 2)))
(define UFO-RADIUS (* 1/2 TANK-WIDTH))
(define WORLD-WIDTH (* 10 TANK-WIDTH))
(define WORLD-HEIGHT (* 15 TANK-WIDTH))

(define TANK-COLOR "darkgreen")
(define CANNON (rectangle CANNON-WIDTH CANNON-HEIGHT "solid" TANK-COLOR))
(define WHEELS (beside/list (make-list WHEEL-AMOUNT (circle WHEEL-RADIUS "solid" "black"))))
(define TANK
  (above
   CANNON
   (overlay/align/offset
    "center" "bottom"
    WHEELS
    0 (- WHEEL-RADIUS)
    (rectangle TANK-WIDTH TANK-HEIGHT "solid" TANK-COLOR))))

(define UFO-COLOR "purple")
(define UFO-GLASS-COLOR "mediumgray")
(define UFO
  (overlay
   (rectangle (* UFO-RADIUS 4) (* 1/8 UFO-RADIUS) "solid" UFO-COLOR)
   (above
    (wedge UFO-RADIUS 180 "solid" UFO-GLASS-COLOR)
    (rotate 180 (wedge UFO-RADIUS 180 "solid" UFO-COLOR)))))

(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT))

(define-struct game [tank ufo])
(define-struct tank [x dx])
; (make-tank Number Number)
;
; x: Center of tank. In range [MIN-TANK-X, MAX-TANK-X]
; dx: Tank's velocity (negative is left, 0 is stopped, positive is right).
(define MIN-TANK-X (+ (/ TANK-WIDTH 2) 1))
(define MAX-TANK-X (- WORLD-WIDTH (/ TANK-WIDTH 2) 1))
(define starting-tank (make-tank (/ WORLD-WIDTH 2) 5))

(define-struct ufo [x y])
; (make-ufo Number Number)
;
; x coordinate: Center of UFO. In range [MIN-UFO-X, MAX-UFO-X]
; y coordinate: Center of UFO. In range [MIN-UFO-Y, MAX-UFO-Y]
(define MIN-UFO-X (+ (/ (image-width UFO) 2) 1))
(define MAX-UFO-X (- WORLD-WIDTH (/ (image-width UFO) 2) 1))
(define MIN-UFO-Y (+ (/ (image-height UFO) 2) 1))
(define MAX-UFO-Y (- WORLD-HEIGHT (/ (image-height UFO) 2) 1))
(define starting-ufo (make-ufo (/ WORLD-WIDTH 2) MIN-UFO-Y))

(define game-start-state (make-game starting-tank starting-ufo))

; space-invader: Game -> Game
; Runs the Space Invader game
(define (space-invader g)
  (big-bang g
            [to-draw render-game]
            [on-tick update-game]
            [on-key handle-key]
            ;; [stop-when "_"]
            ))

; render-game: Game -> Image
; Renders the space invader game state
(check-expect (render-game game-start-state)
              (render-ufo
               (game-ufo game-start-state)
               (render-tank (game-tank game-start-state) BACKGROUND)))
(define (render-game g)
  (render-ufo (game-ufo g) (render-tank (game-tank g) BACKGROUND)))

; render-ufo: UFO Image -> Image
; Places UFO at `img`
(check-expect (render-ufo starting-ufo BACKGROUND)
              (place-image UFO (ufo-x starting-ufo) (ufo-y starting-ufo) BACKGROUND))
(define (render-ufo u img)
  (place-image UFO
               (ufo-x u) (ufo-y u)
               img))

; render-tank: Tank Image -> Image
; Places TANK at the bottom of `img`, at the coordinate of `t`
(check-expect (render-tank starting-tank BACKGROUND)
              (place-image TANK
                           (tank-x starting-tank)
                           (- WORLD-HEIGHT (/ (image-height TANK) 2))
                           BACKGROUND))
(define (render-tank t img)
  (place-image TANK
               (tank-x t)
               (- WORLD-HEIGHT (/ (image-height TANK) 2))
               img))

; update-game: Game -> Game
; Updates the game state, moving its objects
(check-expect (update-game game-start-state)
              (game-up-tank game-start-state (update-tank (game-tank game-start-state))))
(define (update-game g)
  (game-up-tank g (update-tank (game-tank g))))

; update-tank: Tank -> Tank
; Creates a new tank where x is `t`'s x+dx, within the bounds of `x` (see struct definition)
(check-expect (update-tank (make-tank MIN-TANK-X -1)) (make-tank MIN-TANK-X -1))
(check-expect (update-tank (make-tank (- MAX-TANK-X 1) 3)) (make-tank MAX-TANK-X 3))
(check-expect (update-tank starting-tank)
              (make-tank (+ (tank-x starting-tank) (tank-dx starting-tank))
                         (tank-dx starting-tank)))
(define (update-tank t)
  (make-tank
   (cond [(negative? (tank-dx t)) (max MIN-TANK-X (+ (tank-x t) (tank-dx t)))]
         [else (min MAX-TANK-X (+ (tank-x t) (tank-dx t)))])
   (tank-dx t)))

; handle-key: Game KeyEvent -> Game
; Handles key presses for the game
;
; "left": moves tank left
; "right": moves tank right
(check-expect (handle-key game-start-state "a") game-start-state)
(check-expect (handle-key game-start-state "left")
              (game-up-tank game-start-state (tank-change-dx (game-tank game-start-state))))
(check-expect (handle-key game-start-state "right") game-start-state)
(define (handle-key g ke)
  (cond [(or (and (key=? "left" ke) (not (tank-left? (game-tank g))))
             (and (key=? "right" ke) (not (tank-right? (game-tank g)))))
         (game-up-tank g (tank-change-dx (game-tank g)))]
        [else g]))

; game-up-tank: Game Tank -> Game
; Creates a new Game state, with `tank` as the new tank
(check-expect (game-up-tank game-start-state (make-tank MIN-TANK-X 4))
              (make-game (make-tank MIN-TANK-X 4) (game-ufo game-start-state)))
(define (game-up-tank g t)
  (make-game t (game-ufo g)))

; tank-change-dx: Tank -> Tank
; Flips the direction of `tank`
(check-expect (tank-change-dx (make-tank 2 4)) (make-tank 2 -4))
(check-expect (tank-change-dx (make-tank 2 -3)) (make-tank 2 3))
(define (tank-change-dx t)
  (make-tank (tank-x t) (* -1 (tank-dx t))))

; tank-left?: Tank -> Boolean
; True if the tank's moving left
(check-expect (tank-left? starting-tank) #false)
(check-expect (tank-left? (make-tank MAX-TANK-X -3)) #true)
(define (tank-left? t)
  (negative? (tank-dx t)))

; tank-right?: Tank -> Boolean
; True if the tank's moving right
(check-expect (tank-right? starting-tank) #true)
(check-expect (tank-right? (make-tank MAX-TANK-X -3)) #false)
(define (tank-right? t)
  (positive? (tank-dx t)))
