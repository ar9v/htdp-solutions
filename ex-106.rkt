#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; In chapter 5.11 we discussed the creation of virtual pets that come with happiness
;;; gauges. One of the virtual pets is a cat; the other one, a chameleon. Each program
;;; is dedicated to a single pet, however.
;;;
;;; Design the `cat-cham` world program. Given both a location and an animal, it walks
;;; the latter across the canvas, starting from the given location. Here is the chosen
;;; data representation for animals:

; a VAnimal is either
; -- a VCat
; -- a VCham

;;; where VCat and VCham are your data definitions from exercises 88 and 92.

;;; Given that VAnimal is a collection of world states, you need to design
;;; - a rendering function from VAnimal to Image
;;; - a function for handling clock ticks, from VAnimal to VAnimal; and
;;; - a function for dealing with key events so that you can feed and pet and colorize
;;;   your animal -- as applicable.
;;;
;;; It remains impossible to change the color of a cat or to pet a chameleon.

;;; A: Well, this is awkward. I (almost) did this in Exercise 92 (except I used a
;;; product type, not a sum type), since I apparently misunderstood the assignment.

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

; a VAnimalType is either:
; -- CAT-TYPE
; -- CHAM-TYPE
(define CAT-TYPE "cat")
(define CHAM-TYPE "cham")

; cat-cham: VAnimalType Number -> VAnimal
; Kicks off the Virtual Pet World, rendering the type of VAnimal specified by `vatype` at
; location `loc`
(define (cat-cham vatype loc)
  (big-bang
   (cond [(equal? vatype CAT-TYPE) (make-vcat loc RIGHT MAX-HAPPINESS)]
         [(equal? vatype CHAM-TYPE) (make-vcham loc "white" MAX-HAPPINESS)])
   [to-draw render]
   [on-tick update-animal CLOCK-TICK-RATE]
   [on-key handle-key]
   [stop-when unhappiest? render]))

; render: VAnimal -> Image
; Places VAnimal `a` in World at its `x` in a BACKGROUND with a GAUGE that reflects its
; happiness level
(define (render a)
  (cond [(vcat? a)
         (render-cat a (render-gauge (vcat-happiness a)
                                     BACKGROUND))]
        [(vcham? a)
         (render-cham a (render-gauge (vcham-happiness a)
                                      BACKGROUND))]))

; render-cham: VCham Image -> Image
; Given `cham`, produces an image of CHAM with `cham`'s color in `background`
(define (render-cham cham background)
  (place-image
   (overlay CHAM (rectangle CHAM-WIDTH CHAM-HEIGHT "solid" (vcham-color cham)))
   (vcham-x cham) Y-CHAM
   background))

; render-cat: VCat Image -> Image
; Returns either CAT or CAT2, depending on whether (vcat-x cat) is odd or even; on
; `background`
(define (render-cat cat background)
  (place-image (if (odd? (vcat-x cat)) CAT CAT2)
               (vcat-x cat) Y-CAT
               background))

; render-gauge: Happiness Image -> Image
; Overlays a gauge that's filled up to Happiness h, proportional to MAX-HAPPINESS, on
; center/top of `background`
(define (render-gauge h background)
  (overlay/align
   "center" "top"
   (place-image/align
    (rectangle (* (/ h MAX-HAPPINESS) (- GAUGE-WIDTH 2))
               (- GAUGE-HEIGHT 2)
               "solid"
               GAUGE-COLOR)
    1 1
    "left" "top"
    (rectangle GAUGE-WIDTH GAUGE-HEIGHT "solid" "black"))
   background))

; update-animal: VAnimal -> VAnimal
(define (update-animal a)
  (cond [(vcat? a) (update-cat a)]
        [(vcham? a) (update-cham a)]))

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

; handle-key: VAnimal KeyEvent -> VAnimal
; Return a new VAnimal depending on KeyEvent `ke`, which dispatches to each pet's key
; handler
(define (handle-key a ke)
  (cond [(vcat? a) (cat-handle-key a ke)]
        [(vcham? a) (cham-handle-key a ke)]))

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

; unhappiest?: VAnimal -> Boolean
; Returns true if VAnimal a has a happiness of 0
(define (unhappiest? a)
  (<= (cond [(vcat? a) (vcat-happiness a)]
            [(vcham? a) (vcham-happiness a)])
      0))
