#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; Design a world program that maintains and displays a "happiness gauge". Let's call it
;; `gauge-prog`, and let's agree that the program consumes the maximum level of happiness.
;; The gauge display starts with the maximum score, and with each clock tick, happiness
;; decreases by -0.1; it never falls below 0, the minimum happiness score. Every time
;; the down arrow key is pressed, happiness increases by 1/5; every time the up arrow
;; is pressed, happiness jumps by 1/3.
;;
;; To show the level of happiness, we use a scene with a solid, red rectangle with a black
;; frame. For a happiness level of 0, the red bar should be gone; for the maximum happiness
;; level of 100, the bar should go all the way across the scene.

;;; Level is a Number
;;; interpretation: a Level is a Real Number between 0 and MAX-LEVEL

;;; Physical Constants
(define MAX-LEVEL 40)
(define GAUGE-WIDTH MAX-LEVEL)
(define GAUGE-HEIGHT 30)
(define GAUGE-BORDER-WIDTH (+ GAUGE-WIDTH 2))
(define GAUGE-BORDER-HEIGHT (+ GAUGE-HEIGHT 2))
(define DELTA 0.1)
(define NORMAL-BOOST 1/5)
(define BIG-BOOST 1/3)

;;; Graphical Constants
(define BACKGROUND (empty-scene GAUGE-BORDER-WIDTH GAUGE-BORDER-HEIGHT))

;;; gauge-prog: Level -> Level
;;; The function that renders the level of happiness
(define (gauge-prog lvl)
  (big-bang lvl
            [to-draw render]
            [on-tick tock]
            [on-key key-handler]))

;;; render: Level -> Image
;;; Renders the gauge
(check-expect (render 0) BACKGROUND)
(check-expect
 (render (/ MAX-LEVEL 2))
 (place-image/align (rectangle (/ MAX-LEVEL 2) GAUGE-HEIGHT "solid" "red")
              1 1
              "left" "top"
              BACKGROUND))
(check-expect
 (render MAX-LEVEL)
 (place-image/align (rectangle MAX-LEVEL GAUGE-HEIGHT "solid" "red")
                    1 1
                    "left" "top"
                    BACKGROUND))
(define (render lvl)
  ;; TODO: maybe refactor this align, since the coordinates 1 1 are magic numbers
  (place-image/align (rectangle lvl GAUGE-HEIGHT "solid" "red")
                     1 1
                     "left" "top"
                     BACKGROUND))

;;; tock: Level -> Level
;;; Decreases the Level by DELTA
(check-expect (tock 0) 0)
(check-expect (tock 20) (- 20 DELTA))
(check-expect (tock MAX-LEVEL) (- MAX-LEVEL DELTA))
(define (tock lvl)
  (if (= lvl 0) 0 (- lvl DELTA)))


;;; key-handler: Level KeyEvent -> Level
;;; Returns a new Level of happiness depending on `ke`, the key event
(check-expect (key-handler 0 "down") 1/5)
(check-expect (key-handler 0 "up") 1/3)
(check-expect (key-handler 0 "q") 0)
(define (key-handler lvl ke)
  (+ lvl
     (cond [(string=? ke "down") NORMAL-BOOST]
           [(string=? ke "up") BIG-BOOST]
           [else 0])))
