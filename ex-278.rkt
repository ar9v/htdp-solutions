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

(define ITEM-SIZE 25)
(define SEGMENTS-PER-SIDE 10)
(define WORLD-SIZE (* ITEM-SIZE SEGMENTS-PER-SIDE))

(define WORM
  (overlay (square (- ITEM-SIZE 5) "solid" "lightpink")
           (square ITEM-SIZE "solid" "black")))
(define FOOD (square ITEM-SIZE "solid" "brown"))
(define CANVAS (empty-scene WORLD-SIZE WORLD-SIZE))
(define-struct worm [posns direction])

; a Worm is a structure
;  (make-worm [Posn] Direction)
;
; interpretation:
; (make-worm ps d) represents a worm whose segments are at positions `ps`,
; moving LEFT, RIGHT, UP, or DOWN.
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

; build-worm: Posn Number Direction -> Worm
; Creates a Worm instance with a head at `posn` and `n` segments
(check-expect (build-worm (make-posn 0 0) 2 UP)
              (make-worm (list (make-posn 0 1) (make-posn 0 0)) UP))
(check-expect (build-worm (make-posn 0 0) 3 LEFT)
              (make-worm (list (make-posn 2 0) (make-posn 1 0) (make-posn 0 0)) LEFT))
(define (build-worm p n d)
  (local [(define dx (cond [(equal? d RIGHT) 1] [(equal? d LEFT) -1] [else 0]))
          (define dy (cond [(equal? d DOWN)  1] [(equal? d UP)   -1] [else 0]))
          (define (make-segment i)
            (let ([segments-to-head (- (sub1 n) i)])
              (make-posn (- (posn-x p) (* segments-to-head dx))
                         (- (posn-y p) (* segments-to-head dy)))))]
    (make-worm (build-list n make-segment) d)))

(define worm-right (make-worm (list (make-posn 0 0)) RIGHT))
(define worm-up (make-worm (list (make-posn 0 1) (make-posn 0 0)) UP))

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

; worm-game: Game -> Game
; Runs the worm game
(define (worm-game g)
  (big-bang g
            [to-draw render-game]))

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
