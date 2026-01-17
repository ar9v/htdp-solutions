#lang htdp/isl

(require 2htdp/image)
(require 2htdp/universe)

;;; Chapter 12.5 explains how another one of the oldest computer games work.
;;; The game features a worm that moves at a constant speed in a player-controlled
;;; direction. When it encounters food, it eats the food and grows. When it runs into the
;;; wall or into itself, the game is over.
;;;
;;; This project can also benefit from the abstract list-processing functions
;;; in ISL. Look for places to use them and replace existing code, a piece at a
;;; time. Tests will ensure that you aren’t introducing mistakes.

(define ITEM-SIZE 20)
(define STARTING-SIZE 2)
(define SEGMENTS-PER-SIDE 30)
(define DEFAULT-FRAME-RATE 1/15)
(define WORLD-SIZE (* ITEM-SIZE SEGMENTS-PER-SIDE))

(define WORM
  (overlay (square (- ITEM-SIZE 5) "solid" "lightpink")
           (square ITEM-SIZE "solid" "black")))
(define FOOD (square ITEM-SIZE "solid" "brown"))
(define CANVAS (empty-scene WORLD-SIZE WORLD-SIZE))

(define-struct worm [posns directions])
; a Worm is a structure
;  (make-worm [Posn] [NonEmptyList-of Direction])
;
; interpretation:
; (make-worm ps ds) represents a worm whose segments are at positions `ps`,
; moving LEFT, RIGHT, UP, or DOWN according to the last element of `ds`.
;
; A position represents the upper-left coordinate of a segment, and as such
; each coordinate can only be in the range [0, SEGMENTS-PER-SIDE)
;
; assumptions:
;  - The head of the worm is the last element in `ps`.
;  - Each position only differs from its predecessor in one axis
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")

; direction->posn: Direction -> Posn
; Given a Direction, return the unit vector it represents
(define (direction->posn d)
  (cond [(equal? d UP) (make-posn 0 -1)]
        [(equal? d RIGHT) (make-posn 1 0)]
        [(equal? d DOWN) (make-posn 0 1)]
        [(equal? d LEFT) (make-posn -1 0)]))

; posn-add: Posn Posn -> Posn
; Adds the two posns coordinate-wise
(define (posn-add p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))

; posn-sub: Posn Posn -> Posn
; Adds the two posns coordinate-wise
(define (posn-sub p1 p2)
  (make-posn (- (posn-x p1) (posn-x p2)) (- (posn-y p1) (posn-y p2))))

; posn-scale: Posn Number -> Posn
; Scales the given `posn` by `n`; i.e. it produces a new posn where each component is
; the component times `n`
(define (posn-scale p n)
  (make-posn (* (posn-x p) n) (* (posn-y p) n)))

; build-worm: Posn Number Direction -> Worm
; Creates a Worm instance with a head at `posn` and `n` segments
(check-expect (build-worm (make-posn 0 0) 2 UP)
              (make-worm (list (make-posn 0 1) (make-posn 0 0)) (list UP)))
(check-expect (build-worm (make-posn 0 0) 3 LEFT)
              (make-worm (list (make-posn 2 0) (make-posn 1 0) (make-posn 0 0)) (list LEFT)))
(define (build-worm p n d)
  (local [(define direction-vector (direction->posn d))
          (define (make-segment i)
            (local [(define segments-to-head (- (sub1 n) i))]
              (posn-sub p (posn-scale direction-vector segments-to-head))))]
    (make-worm (build-list n make-segment) (list d))))

(define worm-right (build-worm (make-posn 1 0) STARTING-SIZE RIGHT))
(define worm-up (make-worm (list (make-posn 0 1) (make-posn 0 0)) (list UP)))
(define worm-multiple-dirs (make-worm (list (make-posn 3 4)) (list UP LEFT)))

(define-struct game [worm food])
; a Game is a structure
;   (make-game Worm Posn)
;
; interpretation: (make-game w fp) represents a Worm Game with a worm `w` and a piece of
; food placed in `fp`

(define dummy-initial-game-state
  (make-game worm-right
             (make-posn (sub1 SEGMENTS-PER-SIDE)
                        (sub1 SEGMENTS-PER-SIDE))))

; worm-game: Game Number -> Game
; Runs the worm game at a rate of `r` (frames per clock tick)
(define (worm-game g r)
  (big-bang g
            [to-draw render-game]
            [on-tick update-game r]
            [on-key handle-key]
            [stop-when game-over? render-game-final]))

; render-game-final: Game -> Image
; Renders the final state, with a game over message and the final score
(check-expect (render-game-final dummy-initial-game-state)
              (overlay (game-over-banner dummy-initial-game-state)
                       (render-game dummy-initial-game-state)))
(define (render-game-final g)
  (overlay (game-over-banner g) (render-game g)))

; render-game: Game -> Image
; Render the game state in CANVAS
(check-expect (render-game dummy-initial-game-state)
              (render-worm (game-worm dummy-initial-game-state)
                           (render-food (game-food dummy-initial-game-state)
                                        CANVAS)))
(define (render-game g)
  (render-worm (game-worm g) (render-food (game-food g) CANVAS)))

; render-worm: Worm Image -> Image
; places WORM in `img`, using `worm`'s coordinates
(check-expect (render-worm worm-right CANVAS)
              (render-worm-posns (worm-posns worm-right) CANVAS))
(define (render-worm w img)
  (render-worm-posns (worm-posns w) img))

; render-worm-posns: [List-of Posn] Image -> Image
; Place copies of WORM in `img` using each Posn in `posns`'s coordinates
(check-expect (render-worm-posns (worm-posns worm-up) CANVAS)
              (place-game-image WORM 0 1 (place-game-image WORM 0 0 CANVAS)))
(define (render-worm-posns ps img)
  (local [(define (combine p i) (place-game-image WORM (posn-x p) (posn-y p) i))]
    (foldr combine img ps)))

; render-food: Food Image -> Image
(check-expect (render-food (game-food dummy-initial-game-state) CANVAS)
              (local [(define fx (posn-x (game-food dummy-initial-game-state)))
                      (define fy (posn-y (game-food dummy-initial-game-state)))]
                (place-game-image FOOD fx fy CANVAS)))
(define (render-food f img)
  (place-game-image FOOD (posn-x f) (posn-y f) img))

; game-over-banner: Game -> Image
; Produces a game over text overlayed on top of a semi-transparent rectangle
(define (game-over-banner g)
  (local [(define FONT-SIZE (* ITEM-SIZE 2))
          (define FONT-COLOR "white")
          (define score-string (number->string (- (length (worm-posns (game-worm g)))
                                                  STARTING-SIZE)))
          (define game-over-text
            (above (text "Game Over" FONT-SIZE FONT-COLOR)
                   (text (string-append "Score: " score-string) FONT-SIZE FONT-COLOR)))
          (define banner-bg
            (rectangle (* 2/3 WORLD-SIZE)
                       (+ (image-height game-over-text) 10)
                       "solid"
                       (make-color 0 0 0 170)))]
    (overlay game-over-text banner-bg)))

; place-game-image: Image Number Number Image -> Image
; Wrapper around `place-image/align`, since we intend to always align on "top" "left"
; and scale the coordinates depending on ITEM-SIZE
(check-expect (place-game-image WORM 2 1 CANVAS)
              (place-image/align WORM
                                 (* 2 ITEM-SIZE) ITEM-SIZE
                                 "left" "top"
                                 CANVAS))
(define (place-game-image img x y bg)
  (place-image/align img (* ITEM-SIZE x) (* ITEM-SIZE y) "left" "top" bg))

; update-game: Game -> Game
; updates the game state
(check-expect (update-game dummy-initial-game-state)
              (game-up-worm dummy-initial-game-state
                            (update-worm (game-worm dummy-initial-game-state))))
(define (update-game g)
  (local [(define updated-worm (update-worm (game-worm g)))]
    (if (worm-eat-food? g)
        (game-up-food
         (game-up-worm g (expand-worm updated-worm))
         (create-food (game-food g)))
        (game-up-worm g updated-worm))))

; update-worm: Worm -> Worm
; Moves the worm
(check-expect (update-worm worm-right)
              (make-worm (list (make-posn 1 0) (make-posn 2 0)) (list RIGHT)))
(check-expect (update-worm worm-up)
              (make-worm (list (make-posn 0 0) (make-posn 0 -1)) (list UP)))
(check-expect (update-worm worm-multiple-dirs)
              (make-worm (list (make-posn 3 3)) (list LEFT)))
(define (update-worm w)
  (worm-next-direction
   (worm-up-posns w (rest (worm-posns (expand-worm w))))))

; game-up-food: Game Posn -> Game
; Creates a new Game state with a new piece of food in position `p`
(check-expect (game-up-food dummy-initial-game-state (make-posn 3 3))
              (make-game (game-worm dummy-initial-game-state) (make-posn 3 3)))
(define (game-up-food g p)
  (make-game (game-worm g) p))

; game-up-worm: Game Worm -> Game
; Creates a new Game state with a new Worm `w`
(check-expect (game-up-worm dummy-initial-game-state
                            (build-worm (make-posn 0 5) 5 DOWN))
              (make-game (build-worm (make-posn 0 5) 5 DOWN)
                         (game-food dummy-initial-game-state)))
(define (game-up-worm g w)
  (make-game w (game-food g)))

; handle-key: Game KeyEvent -> Game
; Changes the game's worm direction depending on the key
(define (handle-key g ke)
  (if (equal? (game-worm g) (worm-handle-key (game-worm g) ke))
      g
      (make-game (worm-handle-key (game-worm g) ke) (game-food g))))

; worm-handle-key: Worm KeyEvent -> Worm
; Produces a new Worm based on the provided KeyEvent `ke`.
;
; - This ignores all KeyEvents that aren't arrows
; - If a worm is moving RIGHT, it won't be updated to move LEFT and vice-versa
; - A KeyEvent in the same direction the worm is going is ignored
(define (worm-handle-key w ke)
  (if
   (cond [(or (key=? ke "left") (key=? ke "right"))
          (or (equal? (worm-direction w) LEFT) (equal? (worm-direction w) RIGHT))]
         [(or (key=? ke "up") (key=? ke "down"))
          (or (equal? (worm-direction w) UP) (equal? (worm-direction w) DOWN))]
         [else #true])
   w
   (worm-up-directions w (local [(define (up ds) (append ds (list ke)))] up))))

; worm-up-directions: Worm [[List-of Direction] -> [List-of Direction] -> Worm
; Creates a new Worm by applying `updater` to the current directions
(define (worm-up-directions w updater)
  (make-worm (worm-posns w) (updater (worm-directions w))))

; worm-eat-food? Game -> Boolean
; True if `game`'s worm is in the vicinity of its food (read, they have overlapping areas
; within WORM-EATING-DELTA)
(define (worm-eat-food? g)
  (local [(define direction-vector (direction->posn (worm-direction (game-worm g))))]
    (equal? (posn-add (worm-posns-head (game-worm g)) direction-vector)
            (game-food g))))

; Posn -> Posn
; Creates a random Posn in the range ([0, WORLD-WIDTH), [0, WORLD-HEIGHT)).
; Uses `food-check-create` to guarantee that the new Posn is _not_ `p`
(check-satisfied (create-food (make-posn 1 1)) not=-1-1?)
(define (create-food p)
  (create-food-check
   p (make-posn (random (sub1 SEGMENTS-PER-SIDE))
                (random (sub1 SEGMENTS-PER-SIDE)))))

; Posn Posn -> Posn
; generative recursion
; Checks if `p` and `candidate` are equal. If they are, tries generating a new candidate.
(define (create-food-check p candidate)
  (if (equal? p candidate) (create-food p) candidate))

; Posn -> Boolean
; use for testing only
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

; expand-worm: Worm -> Worm
; Given `worm`, produce a new worm with that has an extra segment which respects `worm`'s
; direction
(check-expect (expand-worm worm-right)
              (worm-up-posns worm-right (append (worm-posns worm-right)
                                                (list (posn-add (worm-posns-head worm-right)
                                                                (make-posn 1 0))))))
(define (expand-worm w)
  (worm-up-posns
   w
   (append (worm-posns w)
           (list (posn-add (worm-posns-head w)
                           (direction->posn (worm-direction w)))))))


; worm-posns-head: Worm -> Posn
; Retrieves the head Posn of a worm
(check-expect (worm-posns-head worm-up) (make-posn 0 0))
(define (worm-posns-head worm)
  (last (worm-posns worm)))

; worm-up-posns: Worm [List-of Posn] -> Worm
; Creates a new Worm with positions `posns`
(define (worm-up-posns w ps)
  (make-worm ps (worm-directions w)))

; last: [NonEmptyList-of Any] -> Any
; Retrieves the last element of a non empty list
(check-expect (last (list 1)) 1)
(check-expect (last (list "foo" "bar")) "bar")
(check-expect (last (list #false #false #true)) #true)
(define (last nel)
  (cond [(empty? (rest nel)) (first nel)]
        [else (last (rest nel))]))

; worm-direction: Worm -> Direction
; Gets the first Posn in `worm-directions`
(define (worm-direction w)
  (first (worm-directions w)))

; worm-next-direction: Worm -> Worm
; Returns a new Worm instance, after processing the latest direction
(check-expect (worm-next-direction worm-multiple-dirs)
              (make-worm (worm-posns worm-multiple-dirs)
                         (rest (worm-directions worm-multiple-dirs))))
(check-expect (worm-next-direction worm-right) worm-right)
(define (worm-next-direction w)
  (local [(define rest-directions (rest (worm-directions w)))]
    (make-worm (worm-posns w)
               (if (empty? rest-directions)
                   (worm-directions w)
                   rest-directions))))

; game-over?: Game -> Boolean
; True if the game's worm runs into itself or into a wall
(check-expect
 (game-over? (make-game
              (build-worm (make-posn (random SEGMENTS-PER-SIDE)
                                     (random SEGMENTS-PER-SIDE))
                          3 LEFT)
              (create-food (make-posn 0 0)))) #false)
(define (game-over? g)
  (or (worm-out-of-bounds? (game-worm g))
      (worm-eats-itself? (game-worm g))))

; worm-out-of-bounds?: Worm -> Boolean
; True if the worm has any coordinate:
; - below 0
; - >= SEGMENTS-PER-SIDE
(define (worm-out-of-bounds? w)
  (local [(define head (worm-posns-head w))
          (define (oob? coord) (or (< coord 0) (<= SEGMENTS-PER-SIDE coord)))]
    (ormap oob? (list (posn-x head) (posn-y head)))))

; worm-eats-itself?: Worm -> Boolean
; True if the head of the worm is at thee same position as any of its other segments
(check-expect (worm-eats-itself? worm-right) #false)
(check-expect
 (worm-eats-itself?
  (make-worm (list (make-posn 0 0)
                   (make-posn 1 0)
                   (make-posn 2 0)
                   (make-posn 2 1)
                   (make-posn 1 1)
                   (make-posn 1 0))
             (list UP)))
 #true)
(define (worm-eats-itself? w)
  (member (worm-posns-head w) (but-last (worm-posns w))))


; but-last: [List-of X] -> [List-of X]
; Returns a list made up of `l`'s elements, save for the last one
(check-expect (but-last '()) '())
(check-expect (but-last (list 1)) '())
(check-expect (but-last (list 1 2)) (list 1))
(define (but-last l)
  (cond [(or (empty? l) (empty? (rest l))) '()]
        [else (cons (first l) (but-last (rest l)))]))
