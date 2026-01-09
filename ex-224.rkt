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

; Utilities

; beside/list: [Image] -> Image
; Like 2htdp/image's `beside`, but takes in a list
(check-expect (beside/list '()) empty-image)
(check-expect (beside/list (list TANK CANNON TANK))
              (beside TANK (beside CANNON (beside TANK empty-image))))
(define (beside/list imgs)
  (cond [(empty? imgs) empty-image]
        [(cons? imgs) (beside (first imgs) (beside/list (rest imgs)))]))

; random-between: Number Number -> Number
; Produces a random number lying between `a` and `b`
(check-satisfied (random-between 3 7) between-3-and-7?)
(define (random-between a b)
  (+ a (random (- b a))))

; between-3-and-7?: Number -> Boolean
; For testing the above: checks whether a number is in [3, 7)
(define (between-3-and-7? n)
  (and (<= 3 n) (< n 7)))

; Constants
(define TANK-WIDTH 50)
(define TANK-HEIGHT (* 1/3 TANK-WIDTH))
(define CANNON-WIDTH (* 1/6 TANK-WIDTH))
(define CANNON-HEIGHT TANK-HEIGHT)
(define WHEEL-AMOUNT 5)
(define WHEEL-RADIUS (/ TANK-WIDTH (* WHEEL-AMOUNT 2)))
(define UFO-RADIUS (* 1/2 TANK-WIDTH))
(define UFO-Y-DELTA 10)
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

(define MISSILE-COLOR "black")
(define MISSILE-WIDTH (- CANNON-WIDTH 3))
(define MISSILE-HEIGHT (- CANNON-HEIGHT 1))
(define MISSILE (rectangle MISSILE-WIDTH MISSILE-HEIGHT "solid" MISSILE-COLOR))

(define FONT-SIZE TANK-WIDTH)
(define FONT-COLOR "black")
(define GAME-OVER-TEXT (text "Game Over :-(" FONT-SIZE FONT-COLOR))
(define GAME-WIN-TEXT (text "You win! :-)" FONT-SIZE FONT-COLOR))
(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT))


; Data Definitions
(define-struct game [tank ufo missiles])
(define-struct tank [x dx])
; (make-tank Number Number)
;
; x: Center of tank. In range [MIN-TANK-X, MAX-TANK-X]
; dx: Tank's velocity (negative is left, 0 is stopped, positive is right).
(define MIN-TANK-X (+ (/ TANK-WIDTH 2) 1))
(define MAX-TANK-X (- WORLD-WIDTH (/ TANK-WIDTH 2) 1))
(define starting-tank (make-tank (/ WORLD-WIDTH 2) 5))

(define-struct ufo [x y cx cy])
; (make-ufo Number Number Cooldown Cooldown)
;
; x coordinate: Center of UFO. In range [MIN-UFO-X, MAX-UFO-X]
; y coordinate: Center of UFO. In range [MIN-UFO-Y, MAX-UFO-Y]
; cx: Cooldown for `x` coordinate generation. A number in range [0, MAX-CX]
; cy: Cooldown for `y` coordinate generation. A number in range [0, MAX-CY]
(define MIN-UFO-X (+ (/ (image-width UFO) 2) 1))
(define MAX-UFO-X (- WORLD-WIDTH (/ (image-width UFO) 2) 1))
(define MIN-UFO-Y (+ (/ (image-height UFO) 2) 1))
(define MAX-UFO-Y (- WORLD-HEIGHT (/ (image-height UFO) 2) 1))
(define MAX-CX 24)
(define MAX-CY 2)
(define starting-ufo (make-ufo (/ WORLD-WIDTH 2) MIN-UFO-Y MAX-CX MAX-CY))
(define landed-ufo (make-ufo (- MAX-UFO-X 2) MAX-UFO-Y MAX-CX MAX-CY))

(define-struct missiles [posns cool])
; (make-missiles List<Posn> Cooldown)
;
; posns: A list of where the current fired missiles are in the world
; cool: Cooldown for missile generation. A number in range [0, MAX-MISSILE-COOLDOWN]
(define MAX-MISSILE-COOLDOWN 5)
(define starting-missiles (make-missiles '() MAX-MISSILE-COOLDOWN))
(define fired-missiles (make-missiles (list (make-posn 10 35) (make-posn 50 70))
                                      MAX-MISSILE-COOLDOWN))

(define game-start-state (make-game starting-tank starting-ufo starting-missiles))
(define game-over-state (make-game starting-tank landed-ufo starting-missiles))

; space-invader: Game -> Game
; Runs the Space Invader game
(define (space-invader g)
  (big-bang g
            [to-draw render-game]
            [on-tick update-game]
            [on-key handle-key]
            [stop-when game-over? render-game-final]))

; render-game: Game -> Image
; Renders the space invader game state
(check-expect (render-game game-start-state)
              (render-missiles
               (game-missiles game-start-state)
               (render-ufo
                (game-ufo game-start-state)
                (render-tank (game-tank game-start-state) BACKGROUND))))
(define (render-game g)
  (render-ufo (game-ufo g) (render-tank (game-tank g) BACKGROUND)))

; render-game-final: Game -> Game
; Renders the final state of the game, with a message depending on the outcome
(check-expect (render-game-final game-over-state)
              (place-image GAME-OVER-TEXT
                           (/ WORLD-WIDTH 2)
                           (/ WORLD-HEIGHT 2)
                           (render-game game-over-state)))
(define (render-game-final g)
  (place-image GAME-OVER-TEXT
               (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2)
               (render-game g)))

; render-missiles: Missiles Image -> Image
; Places missiles in `m` on `img`
(check-expect (render-missiles fired-missiles BACKGROUND)
              (place-image MISSILE
                           (posn-x (first (missiles-posns fired-missiles)))
                           (posn-y (first (missiles-posns fired-missiles)))
                           (place-image MISSILE
                                        (posn-x (second (missiles-posns fired-missiles)))
                                        (posn-y (second (missiles-posns fired-missiles)))
                                        BACKGROUND)))
(define (render-missiles ms img)
  (cond [(empty? (missiles-posns ms)) img]
        [else (place-image MISSILE
                           (posn-x (first (missiles-posns ms)))
                           (posn-y (first (missiles-posns ms)))
                           (render-missiles
                            (missiles-up-posns ms (rest (missiles-posns ms)))
                            img))]))

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
              (game-up-ufo (game-up-tank game-start-state
                                         (update-tank (game-tank game-start-state)))
                           (update-ufo (game-ufo game-start-state))))
(define (update-game g)
  (game-up-ufo (game-up-tank g (update-tank (game-tank g)))
               (update-ufo (game-ufo g))))

; update-ufo: UFO -> UFO
; Creates a new UFO in a random x and UFO-Y-DELTA units down if the cooldowns for each
; coordinate have elapsed.
(check-expect (update-ufo starting-ufo)
              (make-ufo (ufo-x starting-ufo)
                        (ufo-y starting-ufo)
                        (sub1 (ufo-cx starting-ufo))
                        (sub1 (ufo-cy starting-ufo))))
(check-random (update-ufo (make-ufo MIN-UFO-X (/ MAX-UFO-Y 2) 0 0))
              (make-ufo (random-between MIN-UFO-X MAX-UFO-X)
                        (+ (/ MAX-UFO-Y 2) UFO-Y-DELTA)
                        MAX-CX
                        MAX-CY))
(check-random (update-ufo (make-ufo MIN-UFO-X (/ MAX-UFO-Y 2) 0 1))
              (make-ufo (random-between MIN-UFO-X MAX-UFO-X)
                        (/ MAX-UFO-Y 2)
                        MAX-CX
                        0))
(check-random (update-ufo (make-ufo MIN-UFO-X (/ MAX-UFO-Y 2) 1 0))
              (make-ufo MIN-UFO-X
                        (+ (/ MAX-UFO-Y 2) UFO-Y-DELTA)
                        0
                        MAX-CY))
(check-random (update-ufo (make-ufo MIN-UFO-X MAX-UFO-Y 1 0))
              (make-ufo MIN-UFO-X
                        MAX-UFO-Y
                        0
                        MAX-CY))
(define (update-ufo u)
  (make-ufo
   (if (zero? (ufo-cx u)) (random-between MIN-UFO-X MAX-UFO-X) (ufo-x u))
   (if (zero? (ufo-cy u)) (min MAX-UFO-Y (+ (ufo-y u) UFO-Y-DELTA)) (ufo-y u))
   (if (zero? (ufo-cx u)) MAX-CX (sub1 (ufo-cx u)))
   (if (zero? (ufo-cy u)) MAX-CY (sub1 (ufo-cy u)))))

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

; game-over? Game -> Boolean
; True if the game is over (the UFO has landed or been hit)
(check-expect (game-over? game-start-state)
              (ufo-landed? (game-ufo game-start-state)))
(check-expect (game-over? game-over-state)
              (ufo-landed? (game-ufo game-over-state)))
(define (game-over? g)
  (ufo-landed? (game-ufo g)))

; ufo-landed?: UFO -> Boolean
; Returns true if the UFO is at the lowest point of the world
(check-expect (ufo-landed? starting-ufo) #false)
(check-expect (ufo-landed? landed-ufo) #true)
(define (ufo-landed? u)
  (<= MAX-UFO-Y (ufo-y u)))

; game-up-ufo: Game UFO -> Game
; Creates a new Game state, with `ufo` as the new `game-ufo`
(check-expect (game-up-ufo game-start-state (make-ufo MIN-UFO-X MAX-UFO-Y 0 0))
              (make-game (game-tank game-start-state)
                         (make-ufo MIN-UFO-X MAX-UFO-Y 0 0)
                         (game-missiles game-start-state)))
(define (game-up-ufo g u)
  (make-game (game-tank g) u (game-missiles g)))

; game-up-tank: Game Tank -> Game
; Creates a new Game state, with `tank` as the new tank
(check-expect (game-up-tank game-start-state (make-tank MIN-TANK-X 4))
              (make-game (make-tank MIN-TANK-X 4)
                         (game-ufo game-start-state)
                         (game-missiles game-start-state)))
(define (game-up-tank g t)
  (make-game t (game-ufo g) (game-missiles g)))

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

; missiles-up-posns: Missiles List<Posns> -> Missiles
; Creates a new Missiles instance with the passed in `posns`
(check-expect (missiles-up-posns fired-missiles '())
              (make-missiles '() (missiles-cool fired-missiles)))
(define (missiles-up-posns ms ps)
  (make-missiles ps (missiles-cool ms)))
