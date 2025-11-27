#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Design the `cham` program, which has the chameleon continuously walking across the
;;; canvas from left to right. When it reaches the right end of the canvas, it disappears
;;; and immediately reappears on the left. Like the cat, the chameleon gets hungry from
;;; all the walking, and, as time passes by, this hunger expresses itself as unhappiness.
;;;
;;; For managing the chameleon's happiness gauge, you may reuse the happiness gauge from
;;; the virtual cat. To make the chameleon happy, you feed it (down arrow, two points
;;; only); petting isn't allowed. Of course, like all chameleons, ours can change color,
;;; too: "r" turns it red, "b" blue, and "g" green. Add the chameleon world program to the
;;; virtual cat game and reuse functions from the latter when possible.
;;;
;;; Start with a data definition, VCham, for representing chameleons.

(define CHAM (bitmap/file "images/cham.png"))
(define CAT (bitmap/file "images/cat.png"))
(define CAT2 (bitmap/file "images/cat2.png"))

; Physical Constants
(define CHAM-WIDTH (image-width CHAM))
(define CHAM-HEIGHT (image-height CHAM))
(define CHAM-HEIGHT-HALF (floor (/ CHAM-HEIGHT 2)))
(define CAT-WIDTH (image-width CAT))
(define CAT-WIDTH-HALF (floor (/ CAT-WIDTH 2)))
(define CAT-HEIGHT (image-height CAT))
(define CAT-HEIGHT-HALF (floor (/ CAT-HEIGHT 2)))
(define WORLD-WIDTH (* CHAM-WIDTH 10))
(define WORLD-HEIGHT (* CHAM-HEIGHT 3))
(define GAUGE-WIDTH (floor (/ WORLD-WIDTH 5)))
(define GAUGE-HEIGHT (floor (/ WORLD-WIDTH 20)))
(define Y-CHAM (- WORLD-HEIGHT CHAM-HEIGHT-HALF 1))
(define Y-CAT (- WORLD-HEIGHT CAT-HEIGHT-HALF 1))
(define CLOCK-TICK-RATE 0.08)
(define SPEED 5)
(define DELTA 0.1)
(define SMALL-BOOST 2)
(define NORMAL-BOOST 3)
(define BIG-BOOST 5)

; Graphical Constants
(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT))
(define GAUGE-COLOR "red")

; Happiness is a Number
; interpretation: Happiness represents the happiness "level" or score.
; constraints: It is a number in [0, MAX-HAPPINESS]
(define MAX-HAPPINESS 100)

; a VCham is a structure
;  (make-vcham Number Color Happiness)
;
; interpretation:
; (make-vcham x c h) represents a chameleon at (x, Y) with a color `c`, and with a happiness
; of h
(define-struct vcham [x color happiness])

; Happiness is a Number
; interpretation: Happiness represents the happiness "level" or score.
; constraints: It is a number in [0, MAX-HAPPINESS]

; Direction is one of
; --- LEFT
; --- RIGHT
; interpretation: LEFT represents moving to the left, RIGHT represents moving to the right
(define LEFT -1)
(define RIGHT 1)

; X is a Number in the range [LEFTMOST-X, RIGHTMOST-X]
(define LEFTMOST-X CAT-WIDTH-HALF)
(define RIGHTMOST-X (- WORLD-WIDTH CAT-WIDTH-HALF))

(define-struct vcat [x direction happiness])
; A VCat (Virtual Cat) is a structure:
;   (make-vcat X Direction Happiness)
;
; interpretation:
; (make-vcat x d h) represents a virtual cat whose x-coordinate (at the center) is `x`,
; has a direction d, and that has a happiness score of `h`.

; a World is a structure:
;  (make-world VCham VCat)
;
; interpretation:
; (make-world cham cat) represents a virtual world that has a VCham `cham` and a VCat `cat`
(define-struct world [vcham vcat])

; world-prog: World -> World
(define (world-prog w)
  (big-bang w
            [to-draw render]
            [on-tick update-world CLOCK-TICK-RATE]
            [on-key handle-key]
            [stop-when any-unhappiest? render]))

; render: World -> Image
; Places pets in World at their positions in a BACKGROUND with a GAUGE that reflects their
; happiness level
(define (render world)
  (place-images
   (list (render-cham (world-vcham world))
         (render-cat (world-vcat world))
         (render-gauge (vcham-happiness (world-vcham world)))
         (render-gauge (vcat-happiness (world-vcat world))))
   (list (make-posn (vcham-x (world-vcham world)) Y-CHAM)
         (make-posn (vcat-x (world-vcat world)) Y-CAT)
         (make-posn (* WORLD-WIDTH 1/3) GAUGE-HEIGHT)
         (make-posn (* WORLD-WIDTH 2/3) GAUGE-HEIGHT))
   BACKGROUND))

; render-cham: VCham -> Image
; Given `cham`, produces an image of CHAM with `cham`'s color
(define (render-cham cham)
  (overlay CHAM (rectangle CHAM-WIDTH CHAM-HEIGHT "solid" (vcham-color cham))))

; render-cat: VCat -> Image
; Returns either CAT or CAT2, depending on whether (vcat-x cat) is odd or even.
(define (render-cat cat)
  (if (odd? (vcat-x cat)) CAT CAT2))

; render-gauge: Happiness -> Image
; Renders a gauge that's filled up to Happiness h, proportional to MAX-HAPPINESS
(define (render-gauge h)
  (place-image/align
   (rectangle (* (/ h MAX-HAPPINESS) (- GAUGE-WIDTH 2))
              (- GAUGE-HEIGHT 2)
              "solid"
              GAUGE-COLOR)
   1 1
   "left" "top"
   (rectangle GAUGE-WIDTH GAUGE-HEIGHT "solid" "black")))

; update-world: World -> World
(define (update-world w)
  (make-world (update-cham (world-vcham w))
              (update-cat (world-vcat w))))

; update-cham: VCham -> VCham
; Moves the cham along the background. When it disappears to the right, it reappears on
; the left.
; Also, decreases its happiness by DELTA
(define (update-cham cham)
  (vcham-happiness-dec
   (vcham-x-up cham (modulo (+ (vcham-x cham) SPEED) (+ WORLD-WIDTH CHAM-WIDTH)))
   DELTA))

; update-cat: VCat -> VCat
; Moves the cat along the background. When it reaches one of the ends, it changes
; direction. Also, decreases its happiness by DELTA
(define (update-cat cat)
  (vcat-happiness-dec
   (if (or (and (equal? (vcat-direction cat) LEFT) (equal? (vcat-x cat) LEFTMOST-X))
           (and (equal? (vcat-direction cat) RIGHT) (equal? (vcat-x cat) RIGHTMOST-X)))
       (vcat-direction-change cat)
       (vcat-x-up cat (if (equal? (vcat-direction cat) LEFT)
                          (max LEFTMOST-X (- (vcat-x cat) SPEED))
                          (min RIGHTMOST-X (+ (vcat-x cat) SPEED)))))
   DELTA))

; vcat-happiness-dec: VCat Number -> VCat
; Given a cat, makes a new cat where vcat-happiness is vcat-happiness - d.
(define (vcat-happiness-dec cat d)
  (make-vcat (vcat-x cat)
             (vcat-direction cat)
             (max 0 (- (vcat-happiness cat) d))))

; vcham-happiness-dec: VCham Number -> VCham
; Given a cham, makes a new cham where vcham-happiness is vcham-happiness - d.
(define (vcham-happiness-dec cham d)
  (make-vcham (vcham-x cham)
              (vcham-color cham)
              (max 0 (- (vcham-happiness cham) d))))

; vcham-x-up: VCham Number -> VCham
; Given a cham, makes a new cham where vcham-x is n
(define (vcham-x-up cham n)
  (make-vcham n (vcham-color cham) (vcham-happiness cham)))

; handle-key: World KeyEvent -> World
; Return a new world depending on KeyEvent `ke`, which dispatches to each pet's key handler
(define (handle-key w ke)
  (make-world (cham-handle-key (world-vcham w) ke)
              (cat-handle-key (world-vcat w) ke)))

; cat-handle-key: VCat KeyEvent -> VCat
; Returns a new VCat given `cat`, where depending on the key event, the update to its
; happiness. If the new happiness exceeds MAX-HAPPINESS, just returns MAX-HAPPINESS
(define (cat-handle-key cat ke)
  (make-vcat (vcat-x cat)
             (vcat-direction cat)
             (min MAX-HAPPINESS
                  (+ (vcat-happiness cat)
                     (cond [(key=? ke "down") NORMAL-BOOST]
                           [(key=? ke "up") BIG-BOOST]
                           [else 0])))))

; cham-handle-key: VCham KeyEvent -> VCham
; Returns a new VCham depending on KeyEvent ke.
;
; If `ke` is
; --- `down`: the chameleon is fed, and gains 2 happiness points
; --- `r`, `g` or `b`: the chameleon turns red, green or blue
; --- anything else: the same chameleon is returned
(define (cham-handle-key cham ke)
  (cond [(key=? ke "down")
         (vcham-happiness-up cham
                             (min MAX-HAPPINESS (+ (vcham-happiness cham) SMALL-BOOST)))]
        [(key=? ke "r") (vcham-color-up cham "red")]
        [(key=? ke "g") (vcham-color-up cham "green")]
        [(key=? ke "b") (vcham-color-up cham "blue")]
        [else cham]))

; vcham-happiness-up: VCham Happiness -> VCham
; Given a `cham` and a Happiness `h`, make a new VCham with the same direction, but
; happiness `h`
(define (vcham-happiness-up cham h)
  (make-vcham (vcham-x cham) (vcham-color cham) h))

; vcham-color-up: VCham Color -> VCham
; Given a `cham` and a Color `c`, make a new VCham with the same properties but in color `c`.
(define (vcham-color-up cham c)
  (make-vcham (vcham-x cham) c (vcham-happiness cham)))

; vcat-direction-change: VCat -> VCat
; If VCat is going LEFT, update it to go RIGHT and viceversa
(define (vcat-direction-change cat)
  (make-vcat (vcat-x cat)
             (if (equal? (vcat-direction cat) LEFT) RIGHT LEFT)
             (vcat-happiness cat)))

; vcat-x-up: VCat Number -> VCat
; Given a cat, makes a new cat where vcat-x is n
(define (vcat-x-up cat n)
  (make-vcat n (vcat-direction cat) (vcat-happiness cat)))

; any-unhappiest?: World -> Boolean
; Returns true if any of the pets in the world has a happiness of 0
(define (any-unhappiest? w)
  (or (<= (vcham-happiness (world-vcham w)) 0)
      (<= (vcat-happiness (world-vcat w)) 0)))
