#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Equip your program from exercise 218 with food. At any point in time, the box should
;;; contain one piece of food. To keep things simple, a piece of food is of the same size
;;; as a worm segment. When the worm's head is located at the same position as the food,
;;; the worm eats the food, meaning the worm's tail is extended by one segment. As the
;;; piece of food is eaten, another one shows up at a different location.
;;;
;;; Adding food to the game requires changes to the data representation of world states.
;;; In addition to the worm, the states now also include a representation of the food,
;;; especially its current location. A change to the game representation suggests new
;;; functions for dealing with events, though these functions can reuse the functions for
;;; the worm (from exercise 218) and their test cases. It also means that the tick handler
;;; must not only move the worm; in addition it must manage the eating process and the
;;; creation of new food.
;;;
;;; Your program should place the food randomly within the box. To do so properly, you need
;;; a design technique that you haven't seen before -- so-called generative recursion,
;;; which is introduced in part V -- so we provide these functions in figure 80. Before you
;;; use them, however, explain how these functions work -- assuming MAX is greater than
;;; 1 -- and then formulate purpose statements.
;;;
;;;
;;; HINTS:
;;; 1. One way to interpret "eating" is to say that the head moves where the food used to
;;;    be located and the tail grows by one segment, inserted where the head used to be.
;;;    Why is this interpretation easy to design as a function?
;;;
;;; 2. We found it useful to add a second parameter to the `worm-main` function for this
;;;    last step, a Boolean that determines whether `big-bang` displays the current state
;;;    of the world in a separate window; see the documentation for `state` on how to ask
;;;    for this information.

(define-struct worm [posns direction])
; a Worm is a structure
;  (make-worm [Posn] Direction)
;
; interpretation: (make-worm ps d) represents a worm whose segments are at positions `ps`,
; moving LEFT, RIGHT, UP, or DOWN.
;
; assumptions:
;  - The head of the worm is the last element in `ps`.
;  - Each position only differs from its predecessor in one axis
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")

; worm-posns-head: Worm -> Posn
; Retrieves the head Posn of a worm
(define (worm-posns-head worm)
  (last (worm-posns worm)))

; create-worm: Posn Number Direction -> Worm
; Creates a Worm whose head is at `p`, with `n` additional segments, going in `direction`
(define (create-worm p n direction)
  (make-worm (create-worm-segments p n direction) direction))

; create-worm-segments: Posn Number Direction -> List<Posn>
; Creates a list of `n` Posns (bseides the head), plus the last one, which is `p`.
; Uses `direction` to determine how to vary each successive coordinate.
(check-expect
 (create-worm-segments (make-posn X-CENTER Y-CENTER) 2 RIGHT)
 (list
   (make-posn (- X-CENTER (* 2 WORM-DIAMETER)) Y-CENTER)
   (make-posn (- X-CENTER WORM-DIAMETER) Y-CENTER)
   (make-posn X-CENTER Y-CENTER)))
(define (create-worm-segments p n direction)
  (cond [(zero? n) (list p)]
        [else (cons
               (cond [(equal? direction LEFT)
                      (make-posn (+ (posn-x p) (* n WORM-DIAMETER)) (posn-y p))]
                     [(equal? direction RIGHT)
                      (make-posn (- (posn-x p) (* n WORM-DIAMETER)) (posn-y p))]
                     [(equal? direction UP)
                      (make-posn (posn-x p) (+ (posn-y p) (* n WORM-DIAMETER)))]
                     [(equal? direction DOWN)
                      (make-posn (posn-x p) (- (posn-y p) (* n WORM-DIAMETER)))])
               (create-worm-segments p (- n 1) direction))]))

(define-struct game [worm food])
; a Game is a structure
;   (make-game Worm Posn)
;
; interpretation: (make-game w fp) represents a Worm Game with a worm `w` and a piece of
; food placed in `fp`

(define WORM-RADIUS 5)
(define WORM-DIAMETER (* WORM-RADIUS 2))
(define FOOD-RADIUS WORM-RADIUS)
(define FOOD-DIAMETER (* 2 FOOD-RADIUS))
(define WORM-EATING-DELTA (- (+ WORM-RADIUS FOOD-RADIUS) 1))
(define WORLD-WIDTH (* WORM-DIAMETER 30))
(define WORLD-HEIGHT WORLD-WIDTH)
(define X-CENTER (/ WORLD-WIDTH 2))
(define Y-CENTER (/ WORLD-HEIGHT 2))
(define INITIAL-POSN (make-posn X-CENTER Y-CENTER))

(define WORM (circle WORM-RADIUS "solid" "red"))
(define FOOD (circle FOOD-RADIUS "solid" "brown"))
(define CANVAS (empty-scene WORLD-WIDTH WORLD-HEIGHT))

; worm-main: Number -> Number
; Runs the Worm game. Takes in the clock rate, returns the final length of the worm.
(define (worm-main rate)
  (length
   (worm-posns
    (game-worm (big-bang (make-game (create-worm INITIAL-POSN 2 RIGHT) (make-posn 50 50))
                         [to-draw render-game]
                         [on-tick update-game rate]
                         [on-key change-game-worm-direction]
                         [stop-when game-over? render-final-state])))))

; render-game: Game -> Image
; Render the game state in the CANVAS
(check-expect
 (render-game
  (make-game (make-worm (list (make-posn 10 10) (make-posn 10 20)) DOWN)
             (make-posn 50 50)))
 (render-food
  (make-posn 50 50)
  (render-worm (make-worm (list (make-posn 10 10) (make-posn 10 20)) DOWN))))
(define (render-game game)
  (render-food (game-food game)
               (render-worm (game-worm game))))

; render-food: Posn Image -> Image
; Places FOOD in `food`'s coordinates on top of `img`
(check-expect (render-food (make-posn 20 20) CANVAS)
              (place-image FOOD 20 20 CANVAS))
(define (render-food food img)
  (place-image FOOD (posn-x food) (posn-y food) img))

; render-worm: Worm -> Image
; Places WORM in CANVAS
(check-expect (render-worm (make-worm (list (make-posn 10 10) (make-posn 10 20)) LEFT))
              (place-image WORM
                           10 20
                           (place-image WORM
                                        10 10
                                        CANVAS)))
(define (render-worm w)
  (render-worm-tails (worm-posns w)))

; render-worm-tails: [Posns] -> Image
; Renders a list of worm tails onto CANVAS
(define (render-worm-tails posns)
  (cond [(empty? posns) CANVAS]
        [(cons? posns)
         (place-image WORM
                      (posn-x (first posns))
                      (posn-y (first posns))
                      (render-worm-tails (rest posns)))]))

; update-game: Game -> Game
; Moves the game's worm and checks if the worm has eaten the game's food
(check-expect
 (update-game
  (make-game (create-worm (make-posn 50 50) 2 RIGHT)
             (make-posn (+ 50 WORM-RADIUS FOOD-RADIUS 1) (+ 50 WORM-RADIUS FOOD-RADIUS 1))))
 (make-game (update-worm (create-worm (make-posn 50 50) 2 RIGHT))
            (make-posn (+ 50 WORM-RADIUS FOOD-RADIUS 1) (+ 50 WORM-RADIUS FOOD-RADIUS 1))))
(check-expect
 (update-game
  (make-game (create-worm (make-posn 50 50) 2 RIGHT)
             (make-posn (+ 50 WORM-RADIUS FOOD-RADIUS) (+ 50 WORM-RADIUS FOOD-RADIUS))))
 (make-game (update-worm (create-worm (make-posn 50 50) 2 RIGHT))
            (make-posn (+ 50 WORM-RADIUS FOOD-RADIUS) (+ 50 WORM-RADIUS FOOD-RADIUS))))
(check-random
 (update-game
  (make-game (create-worm (make-posn 50 50) 2 RIGHT)
             (make-posn (- (+ 50 WORM-RADIUS FOOD-RADIUS) 3) 50)))
 (make-game (expand-worm (update-worm (create-worm (make-posn 50 50) 2 RIGHT)))
            (food-create (make-posn (- (+ 50 WORM-RADIUS FOOD-RADIUS) 3) 50))))
(define (update-game game)
  (if (worm-eat-food? game)
      (make-game
       (expand-worm (update-worm (game-worm game)))
       (food-create (game-food game)))
      (make-game
       (update-worm (game-worm game))
       (game-food game))))

; worm-eat-food? Game -> Boolean
; True if `game`'s worm is in the vicinity of its food (read, they have overlapping areas
; within WORM-EATING-DELTA)
(check-expect
 (worm-eat-food?
  (make-game (create-worm (make-posn 50 50) 2 RIGHT)
             (make-posn (+ 50 WORM-RADIUS FOOD-RADIUS 1) (+ 50 WORM-RADIUS FOOD-RADIUS 1))))
 #false)
(check-expect
 (worm-eat-food?
  (make-game (create-worm (make-posn 50 50) 2 RIGHT)
             (make-posn (+ 50 WORM-RADIUS FOOD-RADIUS) (+ 50 WORM-RADIUS FOOD-RADIUS))))
 #false)
(check-expect
 (worm-eat-food?
  (make-game (create-worm (make-posn 50 50) 2 RIGHT)
             (make-posn (+ 50 WORM-EATING-DELTA) 50)))
 #false)
(check-expect
 (worm-eat-food?
  (make-game (create-worm (make-posn 50 50) 2 RIGHT)
             (make-posn (- (+ 50 WORM-EATING-DELTA) 1) 50)))
 #true)
(define (worm-eat-food? game)
  (< (distance (worm-posns-head (game-worm game))
               (game-food game))
     WORM-EATING-DELTA))

; update-worm: Worm -> Worm
; Moves `worm` one diameter in the direction `worm` is heading
(check-expect (update-worm (make-worm (list (make-posn 50 50) (make-posn 50 60)) LEFT))
              (make-worm
               (rest (append (list (make-posn 50 50) (make-posn 50 60))
                             (list (make-posn (- 50 WORM-DIAMETER) 60))))
               LEFT))
(check-expect (update-worm (make-worm (list (make-posn 50 50) (make-posn 50 60)) RIGHT))
              (make-worm
               (rest (append (list (make-posn 50 50) (make-posn 50 60))
                             (list (make-posn (+ 50 WORM-DIAMETER) 60))))
               RIGHT))
(check-expect (update-worm (make-worm (list (make-posn 50 60) (make-posn 50 50)) UP))
              (make-worm
               (rest (append (list (make-posn 50 60) (make-posn 50 50))
                             (list (make-posn 50 (- 50 WORM-DIAMETER)))))
               UP))
(check-expect (update-worm (make-worm (list (make-posn 50 50) (make-posn 50 60)) DOWN))
              (make-worm
               (rest (append (list (make-posn 50 50) (make-posn 50 60))
                             (list (make-posn 50 (+ 60 WORM-DIAMETER)))))
               DOWN))
(define (update-worm w)
  (make-worm
   (rest
    (append (worm-posns w)
            (list (cond [(equal? (worm-direction w) LEFT)
                         (posn-sub (worm-posns-head w) (make-posn WORM-DIAMETER 0))]
                        [(equal? (worm-direction w) RIGHT)
                         (posn-add (worm-posns-head w) (make-posn WORM-DIAMETER 0))]
                        [(equal? (worm-direction w) UP)
                         (posn-sub (worm-posns-head w) (make-posn 0 WORM-DIAMETER))]
                        [(equal? (worm-direction w) DOWN)
                         (posn-add (worm-posns-head w) (make-posn 0 WORM-DIAMETER))]))))
   (worm-direction w)))

; change-game-worm-direction: Game KeyEvent -> Game
; Creates a new game state where the worm's direction has been updated. See
; `change-worm-direction`
(define (change-game-worm-direction game ke)
  (if (equal? (game-worm game) (change-worm-direction (game-worm game) ke))
      game
      (update-game
       (make-game (change-worm-direction (game-worm game) ke) (game-food game)))))

; change-worm-direction: Worm KeyEvent -> Worm
; Changes `worm`'s direction depending on the arrow key pressed; ignores all other keys.
;
; Constraints: A worm moving left cannot turn right and vice versa; same goes for up/down.
(check-expect
 (change-worm-direction (make-worm (list (make-posn 10 10)) LEFT) "left")
 (make-worm (list (make-posn 10 10)) LEFT))
(check-expect
 (change-worm-direction (make-worm (list (make-posn 50 50)) LEFT) "right")
 (make-worm (list (make-posn 50 50)) LEFT))
(check-expect
 (change-worm-direction (make-worm (list (make-posn 50 50)) LEFT) "up")
 (make-worm (list (make-posn 50 50)) UP))
(check-expect
 (change-worm-direction (make-worm (list (make-posn 50 50)) LEFT) "down")
 (make-worm (list (make-posn 50 50)) DOWN))
(check-expect
 (change-worm-direction (make-worm (list (make-posn 50 50)) DOWN) "up")
 (make-worm (list (make-posn 50 50)) DOWN))
(check-expect
 (change-worm-direction (make-worm (list (make-posn 50 50)) RIGHT) "left")
 (make-worm (list (make-posn 50 50)) RIGHT))
(check-expect
 (change-worm-direction (make-worm (list (make-posn 50 50)) UP) "down")
 (make-worm (list (make-posn 50 50)) UP))
(check-expect
 (change-worm-direction (make-worm (list (make-posn 50 50)) LEFT) "a")
 (make-worm (list (make-posn 50 50)) LEFT))
(define (change-worm-direction w ke)
  (if
   (cond [(or (key=? ke "left") (key=? ke "right"))
          (or (equal? (worm-direction w) LEFT) (equal? (worm-direction w) RIGHT))]
         [(or (key=? ke "up") (key=? ke "down"))
          (or (equal? (worm-direction w) UP) (equal? (worm-direction w) DOWN))]
         [else #true])
   w
   (worm-up-direction w ke)))

; game-over?: Game -> Boolean
; Determines whether the Game's worm has run into a wall or itself
(check-expect
 (game-over?
  (make-game (create-worm (make-posn X-CENTER Y-CENTER) 2 RIGHT)
             (make-posn 50 50))) #false)
(check-expect
 (game-over?
  (make-game
   (make-worm (list (make-posn 20 10)
                    (make-posn 30 10)
                    (make-posn 30 20)
                    (make-posn 20 20)
                    (make-posn 20 10))
              UP)
   (make-posn 50 50)))
 #true)
(check-expect
 (game-over?
  (make-game
   (make-worm (list (make-posn (- WORM-RADIUS 1) WORM-DIAMETER)) LEFT)
   (make-posn 50 50)))
 #true)
(check-expect
 (game-over?
  (make-game
   (make-worm (list (make-posn WORM-RADIUS WORM-DIAMETER)) LEFT)
   (make-posn 50 50)))
 #true)
(check-expect
 (game-over?
  (make-game
   (make-worm (list (make-posn (+ 1 WORM-RADIUS) WORM-DIAMETER)) LEFT)
   (make-posn 50 50)))
 #false)
(check-expect
 (game-over?
  (make-game
   (make-worm (list (make-posn (- WORLD-WIDTH WORM-RADIUS 1) WORM-DIAMETER))
              LEFT)
   (make-posn 50 50)))
 #false)
(check-expect
 (game-over?
  (make-game
   (make-worm (list (make-posn (- WORLD-WIDTH WORM-RADIUS) WORM-DIAMETER))
              LEFT)
   (make-posn 50 50)))
 #true)
(check-expect
 (game-over?
  (make-game (make-worm (list (make-posn (- WORLD-WIDTH (- WORM-RADIUS 1)) WORM-DIAMETER))
                        LEFT)
             (make-posn 50 50)))
 #true)
(check-expect
 (game-over?
  (make-game (make-worm (list (make-posn WORM-DIAMETER (- WORM-RADIUS 1)))
                        LEFT)
             (make-posn 50 50)))
 #true)
(check-expect
 (game-over?
  (make-game (make-worm (list (make-posn WORM-DIAMETER WORM-RADIUS))
                        LEFT)
             (make-posn 50 50)))
 #true)
(check-expect
 (game-over?
  (make-game (make-worm (list (make-posn WORM-DIAMETER (+ 1 WORM-RADIUS)))
                        LEFT)
             (make-posn 50 50)))
 #false)
(check-expect
 (game-over?
  (make-game (make-worm (list (make-posn WORM-DIAMETER (- WORLD-HEIGHT WORM-RADIUS 1)))
                        LEFT)
             (make-posn 50 50)))
 #false)
(check-expect
 (game-over?
  (make-game (make-worm (list (make-posn WORM-DIAMETER (- WORLD-HEIGHT WORM-RADIUS)))
                        LEFT)
             (make-posn 50 50)))
 #true)
(check-expect
 (game-over?
  (make-game (make-worm (list (make-posn WORM-DIAMETER (- WORLD-HEIGHT (- WORM-RADIUS 1))))
                        LEFT)
             (make-posn 50 50)))
 #true)
(define (game-over? w)
  (or (worm-hit-wall? (game-worm w))
      (worm-hit-itself? (game-worm w))))

; worm-hit-wall? Worm -> Boolean
; Determines if `w` has hit a CANVAS wall
(check-expect
 (worm-hit-wall? (make-worm (list (make-posn (- WORM-RADIUS 1) WORM-DIAMETER)) LEFT))
 #true)
(check-expect
 (worm-hit-wall? (make-worm (list (make-posn WORM-RADIUS WORM-DIAMETER)) LEFT))
 #true)
(check-expect
 (worm-hit-wall? (make-worm (list (make-posn (+ 1 WORM-RADIUS) WORM-DIAMETER)) LEFT))
 #false)
(check-expect
 (worm-hit-wall? (make-worm (list (make-posn (- WORLD-WIDTH WORM-RADIUS 1) WORM-DIAMETER))
                            LEFT))
 #false)
(check-expect
 (worm-hit-wall? (make-worm (list (make-posn (- WORLD-WIDTH WORM-RADIUS) WORM-DIAMETER))
                            LEFT))
 #true)
(check-expect
 (worm-hit-wall? (make-worm (list (make-posn (- WORLD-WIDTH (- WORM-RADIUS 1))
                                             WORM-DIAMETER))
                            LEFT))
 #true)
(check-expect
 (worm-hit-wall? (make-worm (list (make-posn WORM-DIAMETER (- WORM-RADIUS 1)))
                            LEFT))
 #true)
(check-expect
 (worm-hit-wall? (make-worm (list (make-posn WORM-DIAMETER WORM-RADIUS))
                            LEFT))
 #true)
(check-expect
 (worm-hit-wall? (make-worm (list (make-posn WORM-DIAMETER (+ 1 WORM-RADIUS)))
                            LEFT))
 #false)
(check-expect
 (worm-hit-wall? (make-worm (list (make-posn WORM-DIAMETER (- WORLD-HEIGHT WORM-RADIUS 1)))
                            LEFT))
 #false)
(check-expect
 (worm-hit-wall? (make-worm (list (make-posn WORM-DIAMETER (- WORLD-HEIGHT WORM-RADIUS)))
                            LEFT))
 #true)
(check-expect
 (worm-hit-wall? (make-worm (list (make-posn WORM-DIAMETER
                                             (- WORLD-HEIGHT (- WORM-RADIUS 1))))
                            LEFT))
 #true)
(define (worm-hit-wall? w)
  (or (<= (- (posn-x (worm-posns-head w)) WORM-RADIUS) 0)
      (<= WORLD-WIDTH (+ (posn-x (worm-posns-head w)) WORM-RADIUS))
      (<= WORLD-HEIGHT (+ (posn-y (worm-posns-head w)) WORM-RADIUS))
      (<= (- (posn-y (worm-posns-head w)) WORM-RADIUS) 0)))

; worm-hit-itself?: Worm -> Boolean
; Determines if `w` has run into itself
(check-expect (worm-hit-itself? (make-worm (list (make-posn 20 10)
                                                 (make-posn 30 10)
                                                 (make-posn 30 20)
                                                 (make-posn 20 20)
                                                 (make-posn 20 10))
                                           UP))
              #true)
(check-expect (worm-hit-itself? (create-worm (make-posn X-CENTER Y-CENTER) 2 RIGHT)) #false)
(define (worm-hit-itself? w)
  (member? (worm-posns-head w) (but-last (worm-posns w))))

; render-final-state: Game -> Image
; Renders the game's final state
(check-expect
 (render-final-state
  (make-game (make-worm (list (make-posn 20 10)
                              (make-posn 30 10)
                              (make-posn 30 20)
                              (make-posn 20 20)
                              (make-posn 20 10))
                        UP)
             (make-posn 50 50)))
 (overlay/align
  "left" "bottom"
  (text "worm ate itself" 16 "black")
  (render-game
   (make-game (make-worm (list (make-posn 20 10)
                               (make-posn 30 10)
                               (make-posn 30 20)
                               (make-posn 20 20)
                               (make-posn 20 10))
                         UP)
              (make-posn 50 50)))))
(check-expect
 (render-final-state
  (make-game
   (make-worm (list (make-posn WORM-DIAMETER (- WORLD-HEIGHT (- WORM-RADIUS 1)))) LEFT)
   (make-posn 50 50)))
 (overlay/align
  "left" "bottom"
  (text "worm hit wall" 16 "black")
  (render-game
   (make-game
    (make-worm (list (make-posn WORM-DIAMETER (- WORLD-HEIGHT (- WORM-RADIUS 1)))) LEFT)
    (make-posn 50 50)))))
(define (render-final-state g)
  (overlay/align
   "left" "bottom"
   (text (if (worm-hit-wall? (game-worm g)) "worm hit wall" "worm ate itself") 16 "black")
   (render-game g)))

; worm-up-direction: Worm Direction -> Worm
; Makes a new worm identical to `w`, save for the new provided `direction`
(define (worm-up-direction w direction)
  (make-worm (worm-posns w) direction))

; expand-worm: Worm -> Worm
; Given `worm`, produce a new worm with that has an extra segment which respects `worm`'s
; direction
(define (expand-worm w)
  (make-worm
   (cond [(equal? (worm-direction w) LEFT)
          (cons (make-posn (+ (posn-x (first (worm-posns w))) WORM-DIAMETER)
                           (posn-y (first (worm-posns w))))
                (worm-posns w))]
         [(equal? (worm-direction w) RIGHT)
          (cons (make-posn (- (posn-x (first (worm-posns w))) WORM-DIAMETER)
                           (posn-y (first (worm-posns w))))
                (worm-posns w))]
         [(equal? (worm-direction w) UP)
          (cons (make-posn (posn-x (first (worm-posns w)))
                           (+ (posn-y (first (worm-posns w))) WORM-DIAMETER))
                (worm-posns w))]
         [(equal? (worm-direction w) DOWN)
          (cons (make-posn (posn-x (first (worm-posns w)))
                           (- (posn-y (first (worm-posns w))) WORM-DIAMETER))
                (worm-posns w))])
   (worm-direction w)))

; posn-sub: Posn Posn -> Posn
; subtracts `p2` from `p1`
(check-expect (posn-sub (make-posn 4 2) (make-posn 1 3)) (make-posn 3 -1))
(define (posn-sub p1 p2)
  (make-posn (- (posn-x p1) (posn-x p2)) (- (posn-y p1) (posn-y p2))))

; posn-add: Posn Posn -> Posn
; Creates a new Posn, which is the result of adding p1 and p2 together, component-wise
(check-expect (posn-add (make-posn 4 2) (make-posn 1 3)) (make-posn 5 5))
(define (posn-add p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))

; last: NonEmptyList<Any> -> Any
; Retrieves the last element of a non empty list
(check-expect (last (list 1)) 1)
(check-expect (last (list "foo" "bar")) "bar")
(check-expect (last (list #false #false #true)) #true)
(define (last nel)
  (cond [(empty? (rest nel)) (first nel)]
        [else (last (rest nel))]))

; but-last: List<Any> -> List<Any>
; Returns a list made up of `l`'s elements, save for the last one
(check-expect (but-last '()) '())
(check-expect (but-last (list 1)) '())
(check-expect (but-last (list 1 2)) (list 1))
(define (but-last l)
  (cond [(or (empty? l) (empty? (rest l))) '()]
        [else (cons (first l) (but-last (rest l)))]))

; Posn -> Posn
; Creates a random Posn in the range ([0, WORLD-WIDTH), [0, WORLD-HEIGHT)).
; Uses `food-check-create` to guarantee that the new Posn is _not_ `p`
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (food-check-create
   p (make-posn (+ (random (- WORLD-WIDTH FOOD-DIAMETER)) FOOD-RADIUS)
                (+ (random (- WORLD-HEIGHT FOOD-DIAMETER)) FOOD-RADIUS))))

; Posn Posn -> Posn
; generative recursion
; Checks if `p` and `candidate` are equal. If they are, tries generating a new candidate.
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

; Posn -> Boolean
; use for testing only
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

; distance: Posn Posn -> Number
; Given p1 and p2, return the distance between them
(check-expect (distance (make-posn 0 0) (make-posn 5 0)) 5)
(check-expect (distance (make-posn 0 0) (make-posn 0 5)) 5)
(check-expect (distance (make-posn 1 1) (make-posn 2 2))
              (inexact->exact (sqrt (+ (sqr (- 2 1))
                                       (sqr (- 2 1))))))
(check-expect (distance (make-posn 20 100) (make-posn 22 103))
              (inexact->exact (sqrt (+ (sqr (- 22 20))
                                       (sqr (- 103 100))))))
(define (distance p1 p2)
  (inexact->exact (sqrt (+ (sqr (- (posn-x p2) (posn-x p1)))
                           (sqr (- (posn-y p2) (posn-y p1)))))))


;;; TODO:
;;; -- update key handler to also ignore opposite directions if head is still
;;;    in the same axis as the rest of the body (a quick up-left while going right causes
;;;    a game over, e.g.)
;;; -- render the worm first, the food second (so the animation looks nicer)
;;; -- try representing the worm head first, so we expand the worm using the interpretation
;;;    of hint 1 (would also make the animation look nicer)
