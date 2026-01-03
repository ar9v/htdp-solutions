#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Redesign your program from exercise 217 so that it stops if the worm has run into the
;;; walls of the world or into itself. Display a message like the one in exercise 216 to
;;; explain whether the program stopped because the worm hit the wall or because it ran
;;; into itself.
;;;
;;; HINTS:
;;; 1. To determine whether a worm is going to run into itself, check whether the position
;;;    of the head would coincide with one of its old tail segments if it moved.
;;;
;;; 2. Read up on the `member?` function.

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

(define WORM-RADIUS 5)
(define WORM-DIAMETER (* WORM-RADIUS 2))
(define WORLD-WIDTH (* WORM-DIAMETER 60))
(define WORLD-HEIGHT WORLD-WIDTH)
(define INITIAL-WORM-POSNS
  (list
   (make-posn (- (/ WORLD-WIDTH 2) (* 2 WORM-DIAMETER)) (/ WORLD-HEIGHT 2))
   (make-posn (- (/ WORLD-WIDTH 2) WORM-DIAMETER) (/ WORLD-HEIGHT 2))
   (make-posn (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2))))

(define WORM (circle WORM-RADIUS "solid" "red"))
(define CANVAS (empty-scene WORLD-WIDTH WORLD-HEIGHT))

; worm-main: Number -> Worm
; Runs the Worm game. Takes in the clock rate.
(define (worm-main rate)
  (big-bang (make-worm INITIAL-WORM-POSNS RIGHT)
            [to-draw render-worm]
            [on-tick update-worm rate]
            [on-key change-worm-direction]
            [stop-when game-over? render-final-state]))

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
  (if (equal? (worm-direction w)
              (cond [(key=? ke "left") RIGHT]
                    [(key=? ke "right") LEFT]
                    [(key=? ke "up") DOWN]
                    [(key=? ke "down") UP]
                    [else (worm-direction w)]))
      w
      (worm-up-direction w ke)))

; game-over?: Worm -> Boolean
; Determines whether the Worm has run into a wall or itself
(check-expect (game-over? (make-worm INITIAL-WORM-POSNS RIGHT)) #false)
(check-expect (game-over? (make-worm (list (make-posn 20 10)
                                           (make-posn 30 10)
                                           (make-posn 30 20)
                                           (make-posn 20 20)
                                           (make-posn 20 10))
                                     UP))
              #true)
(check-expect
 (game-over? (make-worm (list (make-posn (- WORM-RADIUS 1) WORM-DIAMETER)) LEFT))
 #true)
(check-expect
 (game-over? (make-worm (list (make-posn WORM-RADIUS WORM-DIAMETER)) LEFT))
 #true)
(check-expect
 (game-over? (make-worm (list (make-posn (+ 1 WORM-RADIUS) WORM-DIAMETER)) LEFT)) #false)
(check-expect
 (game-over? (make-worm (list (make-posn (- WORLD-WIDTH WORM-RADIUS 1) WORM-DIAMETER))
                        LEFT))
 #false)
(check-expect
 (game-over? (make-worm (list (make-posn (- WORLD-WIDTH WORM-RADIUS) WORM-DIAMETER))
                        LEFT))
 #true)
(check-expect
 (game-over? (make-worm (list (make-posn (- WORLD-WIDTH (- WORM-RADIUS 1)) WORM-DIAMETER))
                        LEFT))
 #true)
(check-expect
 (game-over? (make-worm (list (make-posn WORM-DIAMETER (- WORM-RADIUS 1)))
                        LEFT))
 #true)
(check-expect
 (game-over? (make-worm (list (make-posn WORM-DIAMETER WORM-RADIUS))
                        LEFT))
 #true)
(check-expect
 (game-over? (make-worm (list (make-posn WORM-DIAMETER (+ 1 WORM-RADIUS)))
                        LEFT))
 #false)
(check-expect
 (game-over? (make-worm (list (make-posn WORM-DIAMETER (- WORLD-HEIGHT WORM-RADIUS 1)))
                        LEFT))
 #false)
(check-expect
 (game-over? (make-worm (list (make-posn WORM-DIAMETER (- WORLD-HEIGHT WORM-RADIUS)))
                        LEFT))
 #true)
(check-expect
 (game-over? (make-worm (list (make-posn WORM-DIAMETER (- WORLD-HEIGHT (- WORM-RADIUS 1))))
                        LEFT))
 #true)
(define (game-over? w)
  (or (worm-hit-wall? w)
      (worm-hit-itself? w)))

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
(check-expect (worm-hit-itself? (make-worm INITIAL-WORM-POSNS RIGHT)) #false)
(define (worm-hit-itself? w)
  (member? (worm-posns-head w) (but-last (worm-posns w))))

; render-final-state: Worm -> Image
; Renders the game's final state
(check-expect (render-final-state (make-worm (list (make-posn 20 10)
                                                 (make-posn 30 10)
                                                 (make-posn 30 20)
                                                 (make-posn 20 20)
                                                 (make-posn 20 10))
                                             UP))
              (overlay/align
               "left" "bottom"
               (text "worm ate itself" 16 "black")
               (render-worm (make-worm (list (make-posn 20 10)
                                                 (make-posn 30 10)
                                                 (make-posn 30 20)
                                                 (make-posn 20 20)
                                                 (make-posn 20 10))
                                             UP))))
(check-expect
 (render-final-state
  (make-worm (list (make-posn WORM-DIAMETER (- WORLD-HEIGHT (- WORM-RADIUS 1)))) LEFT))
 (overlay/align
   "left" "bottom"
   (text "worm hit wall" 16 "black")
   (render-worm
    (make-worm (list (make-posn WORM-DIAMETER (- WORLD-HEIGHT (- WORM-RADIUS 1)))) LEFT))))
(define (render-final-state w)
  (overlay/align
   "left" "bottom"
   (text (if (worm-hit-wall? w) "worm hit wall" "worm ate itself") 16 "black")
   (render-worm w)))

; worm-up-direction: Worm Direction -> Worm
; Makes a new worm identical to `w`, save for the new provided `direction`
(define (worm-up-direction w direction)
  (make-worm (worm-posns w) direction))

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
