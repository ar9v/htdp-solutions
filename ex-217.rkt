#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Develop a data representation for worms with tails. A worm's tail is a possibly empty
;;; sequence of "connected" segments. Here "connected" means that the coordinates of a
;;; segment differ from those of its predecessor in at most one direction. To keep things
;;; simple, treat all segments -- head and tail segments -- the same.

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

;;; Now modify your program from exercise 215 to accommodate a multi-segment worm. Keep
;;; things simple:
;;;
;;; 1. Your program may render all worm segments as red disks and
;;; 2. Ignore that the worm may run into the wall or itself
;;;
;;; HINT:
;;; One way to realize the worm's movement is to add a segment in the direction in which it
;;; is moving and to delete the last one.

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
            [on-key change-worm-direction]))

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
