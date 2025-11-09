#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; Recall that the word “height” forced us to choose one of
;; two possible interpretations. Now that you have solved the exercises in
;; this section, solve them again using the first interpretation of the word.
;; Compare and contrast the solutions.

;; Ex. 53

; Data Definition
;
; An LR (short for launching rocket) is one of:
; -- "resting"
; -- NonnegativeNumber
; interpretation: "resting" represents a grounded rocket
; a number denotes the height of a rocket in flight
;
; interpretation: NonnegativeNumber is the distance between the ground (the bottom of the
; canvas and the reference point (in this case, the center of the rocket)

;;; Given the following
(define HEIGHT 300) ; distances in pixels
(define WIDTH 100)
(define YDELTA 11)
(define X-CENTER (/ WIDTH 2))

(define SCENE (empty-scene WIDTH HEIGHT))

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define ROCKET (bitmap/file "images/rocket.png"))

(define CENTER (/ (image-height ROCKET) 2))

;;; Info -> Data

;;; When the launching rocket is "resting", its position is
;;; (X-CENTER, HEIGHT)
(place-image ROCKET X-CENTER HEIGHT SCENE)

;;; When the launching rocket is 0, that means that it is positioned at
;;; (X-CENTER, HEIGHT), with the point of reference being the rocket's center
;;;
;;; Since the point of reference is the ROCKET's center, we only see the upper half
(place-image ROCKET X-CENTER HEIGHT SCENE)

;;; When the launching rocket is 50, that means that it is positioned at
;;; (X-CENTER, HEIGHT - 50).
(place-image ROCKET X-CENTER (- HEIGHT 50) SCENE)

;;; When the launching rocket is HEIGHT, that means that it is positioned at
;;; (X-CENTER, 0).
;;;
;;; Since the point of reference is the rocket's center, this technically means that
;;; we only see the ROCKET's lower half
(place-image ROCKET X-CENTER 0 SCENE)

;;; Data -> Info

;;; "resting" -> The rocket is grounded

;;; 0 -> The rocket is at the bottom of the canvas position (with only it's lower half
;;; rendering)

;;; 50 -> The rocket is HEIGHT - 50 pixels away from reaching the top of the canvas, or the
;;; topmost visible part.

;;; HEIGHT -> The rocket is rendered at the top of the canvas, which is the topmost
;;; visible part of the image for us. Only the upper bottom of it can be seen, since the
;;; reference is the rocket's center.

;;; NOTE:
;;; Same caveat as before, we don't shift the images until exercises 54 and beyond.


;; Ex. 54
;;; This answer doesn't change!


;; Ex. 55 and 56
(define (main s)
  (big-bang s
            [to-draw show]
            [on-key launch]
            [on-tick fly 0.5]
            [stop-when out-of-sight?]))

; show: LRCD -> Image
; render the state as a resting or flying rocket
(check-expect (show "resting") (draw-rocket (- HEIGHT CENTER)))

(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              X-CENTER (* 3/4 WIDTH)
              (draw-rocket (- HEIGHT CENTER))))

(check-expect
 (show 53)
 (draw-rocket (- HEIGHT 53 CENTER)))

;;; Now this is the test for when the rocket's left our line of sight.
(check-expect
 (show HEIGHT)
 (draw-rocket (- CENTER)))

(check-expect
 (show 0)
 (draw-rocket (- HEIGHT CENTER)))

(define (show x)
  (cond [(string? x)
         (draw-rocket (- HEIGHT CENTER))]
        [(<= -3 x -1)
         (place-image (text (number->string x) 20 "red")
                      X-CENTER (* 3/4 WIDTH)
                      (draw-rocket (- HEIGHT CENTER)))]
        [(>= x 0)
         (draw-rocket (- HEIGHT x CENTER))]))

;;; draw-rocket: Number -> Image
;;; Renders ROCKET at position (10, `y`) on BACKGROUND
(check-expect (draw-rocket 0) (place-image ROCKET X-CENTER 0 BACKGROUND))
(check-expect (draw-rocket 50) (place-image ROCKET X-CENTER 50 BACKGROUND))
(check-expect (draw-rocket HEIGHT) (place-image ROCKET X-CENTER HEIGHT BACKGROUND))
(define (draw-rocket y)
  (place-image ROCKET X-CENTER y BACKGROUND))

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
(check-expect (fly -1) 0)
(check-expect (fly 10) (+ 10 YDELTA))
(check-expect (fly 22) (+ 22 YDELTA))
(define (fly x)
  (cond [(string? x) x]
        [(<= -3 x -1) (add1 x)]
        [(>= x 0) (+ x YDELTA)]))


;; If you watch the entire launch, you will notice that once the rocket reaches the top
;; something curious happens. Explain. Add a `stop-when` clause to `main2` so that the
;; simulation of the liftoff stops gracefully when the rocket is out of sight.

;;; Answer:
;;;
;;; In this version of the program this actually doesn't happen! Because our state
;;; doesn't loop back to negative numbers. This might be the greatest stregnth of this
;;; interpretation: you have to subtract from the HEIGHT to produce an image, but the code
;;; for `fly` and `out-of-sight?` is simpler!


; out-of-sight?: LRCD -> Boolean
; Returns true when ROCKET is out of sight
(check-expect (out-of-sight? "resting") #false)
(check-expect (out-of-sight? -3) #false)
(check-expect (out-of-sight? -2) #false)
(check-expect (out-of-sight? -1) #false)
(check-expect (out-of-sight? 0) #false)
(check-expect (out-of-sight? 50) #false)
(check-expect (out-of-sight? HEIGHT) #true)
(check-expect (out-of-sight? (+ HEIGHT 5)) #true)
(define (out-of-sight? x)
  (cond [(string? x) #false]
        [(<= -3 x -1) #false]
        [(>= x 0) (<= HEIGHT x)]))
