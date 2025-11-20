#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;;; Modify the happy-cat program from the preceding exercises so that it stops whenever the
;;; cat’s happiness falls to 0.

(define MAX-HAPPINESS 100)

; Happiness is a Number
; interpretation: Happiness represents the happiness "level" or score.
; constraints: It is a number in [0, MAX-HAPPINESS]

(define-struct vcat [x happiness])
; A VCat (Virtual Cat) is a structure:
;   (make-vcat Number Happiness)
;
; interpretation:
; (make-vcat x h) represents a virtual cat whose x-coordinate (at the center) is `x`,
; and that has a happiness score of `h`.

(define happiest-cat (make-vcat 0 MAX-HAPPINESS))
(define unhappiest-cat (make-vcat 15 0))
(define partially-happy-cat (make-vcat 150 (/ MAX-HAPPINESS 2)))

(define CAT (bitmap/file "images/cat.png"))
(define CAT2 (bitmap/file "images/cat2.png"))

; Physical Constants
(define CAT-WIDTH (image-width CAT))
(define CAT-WIDTH-HALF (floor (/ CAT-WIDTH 2)))
(define CAT-HEIGHT (image-height CAT))
(define CAT-HEIGHT-HALF (floor (/ CAT-HEIGHT 2)))
(define WORLD-WIDTH (* CAT-WIDTH 10))
(define WORLD-HEIGHT (* CAT-HEIGHT 3))
(define GAUGE-WIDTH (floor (/ WORLD-WIDTH 5)))
(define GAUGE-HEIGHT (floor (/ WORLD-WIDTH 20)))
(define Y (- WORLD-HEIGHT CAT-HEIGHT-HALF 1))
(define CLOCK-TICK-RATE 0.08)
(define SPEED 5)
(define DELTA 2)
(define NORMAL-BOOST 3)
(define BIG-BOOST 5)

; Graphical Constants
(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT))
(define GAUGE-COLOR "red")

; happy-cat: Number -> VCat
(define (happy-cat x)
  (big-bang (make-vcat x MAX-HAPPINESS)
            [to-draw render]
            [on-tick update-cat CLOCK-TICK-RATE]
            [on-key update-happiness]
            [stop-when unhappiest? render]))

; render: VCat -> Image
; Places (render-cat cat) at position ((vcat-x cat) - CAT-WIDTH-HALF, Y) in a
; BACKGROUND with a GAUGE that reflects (vcat-happiness cat)
(check-expect (render happiest-cat)
              (place-image (render-cat happiest-cat)
               (- (vcat-x happiest-cat) CAT-WIDTH-HALF) Y
               (background-with-gauge (vcat-happiness happiest-cat))))
(check-expect (render unhappiest-cat)
              (place-image (render-cat unhappiest-cat)
                           (- (vcat-x unhappiest-cat) CAT-WIDTH-HALF) Y
                           (background-with-gauge (vcat-happiness unhappiest-cat))))
(check-expect (render partially-happy-cat)
              (place-image (render-cat partially-happy-cat)
                           (- (vcat-x partially-happy-cat) CAT-WIDTH-HALF) Y
                           (background-with-gauge (vcat-happiness partially-happy-cat))))
(define (render cat)
  (place-image (render-cat cat)
               (- (vcat-x cat) CAT-WIDTH-HALF) Y
               (background-with-gauge (vcat-happiness cat))))

; render-cat: VCat -> Image
; Returns either CAT or CAT2, depending on whether (vcat-x cat) is odd or even.
(check-expect (render-cat happiest-cat) CAT2)
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
; Moves the cat along the background. When it disappears to the right, it reappears on
; the left.
; Also, decreases its happiness by DELTA
(check-expect (update-cat partially-happy-cat)
              (vcat-happiness-dec
               (vcat-x-up partially-happy-cat (+ (vcat-x partially-happy-cat) SPEED))
               DELTA))
(check-expect (update-cat (make-vcat (+ WORLD-WIDTH CAT-WIDTH 5) 20))
              (make-vcat (+ 5 SPEED) (- 20 DELTA)))
(define (update-cat cat)
  (vcat-happiness-dec
   (vcat-x-up cat (modulo (+ (vcat-x cat) SPEED) (+ WORLD-WIDTH CAT-WIDTH)))
   DELTA))

; vcat-x-up: VCat Number -> VCat
; Given a cat, makes a new cat where vcat-x is n
(check-expect (vcat-x-up partially-happy-cat 1)
              (make-vcat 1 (vcat-happiness partially-happy-cat)))
(define (vcat-x-up cat n)
  (make-vcat n (vcat-happiness cat)))

; vcat-happiness-dec: VCat Number -> VCat
; Given a cat, makes a new cat where vcat-happiness is vcat-happiness - d.
(check-expect (vcat-happiness-dec unhappiest-cat 2) unhappiest-cat)
(check-expect (vcat-happiness-dec partially-happy-cat 2)
              (make-vcat (vcat-x partially-happy-cat)
                        (- (vcat-happiness partially-happy-cat) 2)))
(define (vcat-happiness-dec cat d)
  (make-vcat (vcat-x cat)
             (max 0 (- (vcat-happiness cat) d))))

; update-happiness: VCat KeyEvent -> VCat
; Returns a new VCat given `cat`, where depending on the key event, the update to its
; happiness. If the new happiness exceeds MAX-HAPPINESS, just returns MAX-HAPPINESS
(check-expect (update-happiness unhappiest-cat "down")
              (make-vcat (vcat-x unhappiest-cat) NORMAL-BOOST))
(check-expect (update-happiness unhappiest-cat "up")
              (make-vcat (vcat-x unhappiest-cat) BIG-BOOST))
(check-expect (update-happiness unhappiest-cat "q")
              (make-vcat (vcat-x unhappiest-cat) 0))
(check-expect (update-happiness happiest-cat "up")
              (make-vcat (vcat-x happiest-cat) MAX-HAPPINESS))
(define (update-happiness cat ke)
  (make-vcat (vcat-x cat)
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
