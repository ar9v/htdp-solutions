#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Extend your structure type definition and data definition from exercise 88 to include
;;; a direction field. Adjust your `happy-cat` program so that the cat moves in the
;;; specified direction. The program should move the cat in the current direction, and
;;; it should turn the cat around when it reaches either end of the scene.

(define CAT (bitmap/file "images/cat.png"))
(define CAT2 (bitmap/file "images/cat2.png"))

; Physical Constants
(define CAT-WIDTH (image-width CAT))
(define CAT-WIDTH-HALF (floor (/ CAT-WIDTH 2)))
(define CAT-HEIGHT (image-height CAT))
(define CAT-HEIGHT-HALF (floor (/ CAT-HEIGHT 2)))
(define WORLD-WIDTH (* CAT-WIDTH 5))
(define WORLD-HEIGHT (* CAT-HEIGHT 2))
(define GAUGE-WIDTH (floor (/ WORLD-WIDTH 4)))
(define GAUGE-HEIGHT (floor (/ WORLD-WIDTH 15)))
(define Y (- WORLD-HEIGHT CAT-HEIGHT-HALF 1))
(define CLOCK-TICK-RATE 0.08)
(define SPEED 5)
(define DELTA 0.1)
(define NORMAL-BOOST 3)
(define BIG-BOOST 5)

(define MAX-HAPPINESS 100)

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

(define happiest-cat (make-vcat CAT-WIDTH-HALF RIGHT MAX-HAPPINESS))
(define unhappiest-cat (make-vcat 15 RIGHT 0))
(define partially-happy-cat (make-vcat 150 LEFT (/ MAX-HAPPINESS 2)))
(define left-moving-cat (make-vcat 150 LEFT (/ MAX-HAPPINESS 2)))

; Graphical Constants
(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT))
(define GAUGE-COLOR "red")

; happy-cat: X -> VCat
(define (happy-cat x)
  (big-bang (make-vcat x RIGHT MAX-HAPPINESS)
            [to-draw render]
            [on-tick update-cat CLOCK-TICK-RATE]
            [on-key update-happiness]
            [stop-when unhappiest? render]))

; render: VCat -> Image
; Places (render-cat cat) at position ((vcat-x cat) - CAT-WIDTH-HALF, Y) in a
; BACKGROUND with a GAUGE that reflects (vcat-happiness cat)
(check-expect (render happiest-cat)
              (place-image (render-cat happiest-cat)
               (vcat-x happiest-cat) Y
               (background-with-gauge (vcat-happiness happiest-cat))))
(check-expect (render unhappiest-cat)
              (place-image (render-cat unhappiest-cat)
                           (vcat-x unhappiest-cat) Y
                           (background-with-gauge (vcat-happiness unhappiest-cat))))
(check-expect (render partially-happy-cat)
              (place-image (render-cat partially-happy-cat)
                           (vcat-x partially-happy-cat) Y
                           (background-with-gauge (vcat-happiness partially-happy-cat))))
(define (render cat)
  (place-image (render-cat cat)
               (vcat-x cat) Y
               (background-with-gauge (vcat-happiness cat))))

; render-cat: VCat -> Image
; Returns either CAT or CAT2, depending on whether (vcat-x cat) is odd or even.
(check-expect (render-cat happiest-cat) (if (odd? CAT-WIDTH-HALF) CAT CAT2))
(check-expect (render-cat unhappiest-cat) CAT)
(check-expect (render-cat partially-happy-cat) CAT2)
(define (render-cat cat)
  (if (odd? (vcat-x cat)) CAT CAT2))

; background-with-gauge: Happiness -> Image
; Renders the background with a gauge on the top right, which reflects Happiness h
(check-expect (background-with-gauge 0)
              (overlay/align "right" "top" (render-gauge 0) BACKGROUND))
(check-expect (background-with-gauge MAX-HAPPINESS)
              (overlay/align "right" "top" (render-gauge MAX-HAPPINESS) BACKGROUND))
(define (background-with-gauge h)
  (overlay/align "right" "top" (render-gauge h) BACKGROUND))

; render-gauge: Happiness -> Image
; Renders a gauge that's filled up to Happiness h, proportional to MAX-HAPPINESS
(check-expect (render-gauge 0) (rectangle GAUGE-WIDTH GAUGE-HEIGHT "solid" "black"))
(check-expect (render-gauge 50)
              (place-image/align
               (rectangle (* (/ 50 MAX-HAPPINESS) (- GAUGE-WIDTH 2))
                          (- GAUGE-HEIGHT 2)
                          "solid"
                          GAUGE-COLOR)
               1 1
               "left" "top"
               (rectangle GAUGE-WIDTH GAUGE-HEIGHT "solid" "black")))
(check-expect (render-gauge MAX-HAPPINESS)
              (place-image/align
               (rectangle (- GAUGE-WIDTH 2) (- GAUGE-HEIGHT 2) "solid" GAUGE-COLOR)
               1 1
               "left" "top"
               (rectangle GAUGE-WIDTH GAUGE-HEIGHT "solid" "black")))
(define (render-gauge h)
  (place-image/align
   (rectangle (* (/ h MAX-HAPPINESS) (- GAUGE-WIDTH 2))
              (- GAUGE-HEIGHT 2)
              "solid"
              GAUGE-COLOR)
   1 1
   "left" "top"
   (rectangle GAUGE-WIDTH GAUGE-HEIGHT "solid" "black")))

; update-cat: VCat -> VCat
; Moves the cat along the background. When it reaches one of the ends, it changes
; direction. Also, decreases its happiness by DELTA
(check-expect (update-cat partially-happy-cat)
              (vcat-happiness-dec
               (vcat-x-up partially-happy-cat
                          (+ (vcat-x partially-happy-cat)
                             (* SPEED (vcat-direction partially-happy-cat))))
               DELTA))
(check-expect (update-cat (make-vcat LEFTMOST-X LEFT 20))
              (vcat-happiness-dec (make-vcat LEFTMOST-X RIGHT 20) DELTA))
(check-expect (update-cat (make-vcat RIGHTMOST-X RIGHT 20))
              (vcat-happiness-dec (make-vcat RIGHTMOST-X LEFT 20) DELTA))
(check-expect (update-cat (make-vcat (+ LEFTMOST-X (sub1 SPEED)) LEFT 20))
              (vcat-happiness-dec (make-vcat LEFTMOST-X LEFT 20) DELTA))
(check-expect (update-cat (make-vcat (add1 (- RIGHTMOST-X SPEED)) RIGHT 20))
              (vcat-happiness-dec (make-vcat RIGHTMOST-X RIGHT 20) DELTA))
(define (update-cat cat)
  (vcat-happiness-dec
   (if (or (and (equal? (vcat-direction cat) LEFT) (equal? (vcat-x cat) LEFTMOST-X))
           (and (equal? (vcat-direction cat) RIGHT) (equal? (vcat-x cat) RIGHTMOST-X)))
       (vcat-direction-change cat)
       (vcat-x-up cat (if (equal? (vcat-direction cat) LEFT)
                             (max LEFTMOST-X (- (vcat-x cat) SPEED))
                             (min RIGHTMOST-X (+ (vcat-x cat) SPEED)))))
   DELTA))

; vcat-x-up: VCat Number -> VCat
; Given a cat, makes a new cat where vcat-x is n
(check-expect (vcat-x-up partially-happy-cat 1)
              (make-vcat 1
                         (vcat-direction partially-happy-cat)
                         (vcat-happiness partially-happy-cat)))
(define (vcat-x-up cat n)
  (make-vcat n (vcat-direction cat) (vcat-happiness cat)))

; vcat-happiness-dec: VCat Number -> VCat
; Given a cat, makes a new cat where vcat-happiness is vcat-happiness - d.
(check-expect (vcat-happiness-dec unhappiest-cat 2) unhappiest-cat)
(check-expect (vcat-happiness-dec partially-happy-cat 2)
              (make-vcat (vcat-x partially-happy-cat)
                         (vcat-direction partially-happy-cat)
                         (- (vcat-happiness partially-happy-cat) 2)))
(define (vcat-happiness-dec cat d)
  (make-vcat (vcat-x cat)
             (vcat-direction cat)
             (max 0 (- (vcat-happiness cat) d))))

; vcat-direction-change: VCat -> VCat
; If VCat is going LEFT, update it to go RIGHT and viceversa
(check-expect (vcat-direction-change happiest-cat)
              (make-vcat (vcat-x happiest-cat)
                         LEFT
                         (vcat-happiness happiest-cat)))
(check-expect (vcat-direction-change left-moving-cat)
              (make-vcat (vcat-x left-moving-cat)
                         RIGHT
                         (vcat-happiness left-moving-cat)))
(define (vcat-direction-change cat)
  (make-vcat (vcat-x cat)
             (if (equal? (vcat-direction cat) LEFT) RIGHT LEFT)
             (vcat-happiness cat)))

; update-happiness: VCat KeyEvent -> VCat
; Returns a new VCat given `cat`, where depending on the key event, the update to its
; happiness. If the new happiness exceeds MAX-HAPPINESS, just returns MAX-HAPPINESS
(check-expect (update-happiness unhappiest-cat "down")
              (make-vcat (vcat-x unhappiest-cat)
                         (vcat-direction unhappiest-cat)
                         NORMAL-BOOST))
(check-expect (update-happiness unhappiest-cat "up")
              (make-vcat (vcat-x unhappiest-cat)
                         (vcat-direction unhappiest-cat)
                         BIG-BOOST))
(check-expect (update-happiness unhappiest-cat "q")
              (make-vcat (vcat-x unhappiest-cat) (vcat-direction unhappiest-cat) 0))
(check-expect (update-happiness happiest-cat "up")
              (make-vcat (vcat-x happiest-cat)
                         (vcat-direction unhappiest-cat)
                         MAX-HAPPINESS))
(define (update-happiness cat ke)
  (make-vcat (vcat-x cat)
             (vcat-direction cat)
             (min MAX-HAPPINESS
                  (+ (vcat-happiness cat)
                     (cond [(key=? ke "down") NORMAL-BOOST]
                           [(key=? ke "up") BIG-BOOST]
                           [else 0])))))

; unhappiest?: VCat -> Boolean
; Returns true when a given cat's happiness is 0
(check-expect (unhappiest? unhappiest-cat) #true)
(check-expect (unhappiest? partially-happy-cat) #false)
(define (unhappiest? cat)
  (<= (vcat-happiness cat) 0))
