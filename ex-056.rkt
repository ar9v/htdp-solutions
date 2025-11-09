#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; Define `main2` so that you can launch the rocket and watch it lift off. Read up on
;; the `on-tick` clause to determine the length of one tick and how to change it.

;;; Constants
(define HEIGHT 300) ; distances in pixels
(define WIDTH 100)
(define YDELTA 11)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define ROCKET (bitmap/file "images/rocket.png"))

(define CENTER (/ (image-height ROCKET) 2))

; main1: LRCD -> LRCD
(define (main1 s)
  (big-bang s
            [to-draw show]
            [on-key launch]))

(define (main2 s)
  (big-bang s
            [to-draw show]
            [on-key launch]
            [on-tick fly 0.5]
            [stop-when out-of-sight?]))

; an LRCD (for launching rocket countdown) is one of:
; -- "resting"
; -- a Number between -3 and -1
; -- a NonnegativeNumber
;
; interpretation: a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)

; show: LRCD -> Image
; render the state as a resting or flying rocket
(check-expect (show "resting") (draw-rocket (- HEIGHT CENTER)))

(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              10 (* 3/4 WIDTH)
              (draw-rocket (- HEIGHT CENTER))))

(check-expect
 (show 53)
 (draw-rocket (- 53 CENTER)))

(check-expect
 (show HEIGHT)
 (draw-rocket (- HEIGHT CENTER)))

;;; This renders the image when the rocket's left our line of sight.
(check-expect
 (show 0)
 (draw-rocket (- 0 CENTER)))

(define (show x)
  (cond [(string? x)
         (draw-rocket (- HEIGHT CENTER))]
        [(<= -3 x -1)
         (place-image (text (number->string x) 20 "red")
                      10 (* 3/4 WIDTH)
                      (draw-rocket (- HEIGHT CENTER)))]
        [(>= x 0)
         (draw-rocket (- x CENTER))]))

;;; draw-rocket: Number -> Image
;;; Renders ROCKET at position (10, `y`) on BACKGROUND
(check-expect (draw-rocket 0) (place-image ROCKET 10 0 BACKGROUND))
(check-expect (draw-rocket 50) (place-image ROCKET 10 50 BACKGROUND))
(check-expect (draw-rocket HEIGHT) (place-image ROCKET 10 HEIGHT BACKGROUND))
(define (draw-rocket y)
  (place-image ROCKET 10 y BACKGROUND))

; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed,
; if the rocket is still resting
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)
(define (launch x ke)
  (cond [(string? x) (if (string=? " " ke) -3 x)]
        [(<= -3 x -1) x]
        [(>= x 0) x]))

; LRCD -> LRCD
; raises the rocket by YDELTA,
; if it is moving already
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) HEIGHT)
(check-expect (fly 10) (max (- 10 YDELTA) 0))
(check-expect (fly 22) (max (- 22 YDELTA) 0))
(define (fly x)
  (cond [(string? x) x]
        [(<= -3 x -1) (if (= x -1) HEIGHT (add1 x))]

        ;; As per the book, either way you handle the `stop-when` logic, this program is
        ;; funky, because we represent the countdown w/ negative numbers, which creates
        ;; ambiguity.
        [(>= x 0) (max (- x YDELTA) 0)]))


;; If you watch the entire launch, you will notice that once the rocket reaches the top
;; something curious happens. Explain. Add a `stop-when` clause to `main2` so that the
;; simulation of the liftoff stops gracefully when the rocket is out of sight.

;;; Answer:
;;;
;;; When the rocket reaches the top, LRCD is 0. Then, `fly` performs `(- 0 YDELTA)`, which
;;; yields the new state of -3. That means that on the next tick, `fly` will pick the
;;; second branch of its cond, thus looping the whole liftoff.


; out-of-sight?: LRCD -> Boolean
; Returns true when ROCKET is out of sight
(check-expect (out-of-sight? "resting") #false)
(check-expect (out-of-sight? -3) #false)
(check-expect (out-of-sight? -2) #false)
(check-expect (out-of-sight? -1) #false)
(check-expect (out-of-sight? 50) #false)
(check-expect (out-of-sight? 0) #true)
(define (out-of-sight? x)
  (cond [(string? x) #false]
        [(<= -3 x -1) #false]

        ;; The idea is that `fly` will at the least produce a 0. When the state is 0, the
        ;; rocket is out of sight because we displace the image by half the ROCKET's height.
        [(>= x 0) (= x 0)]))
