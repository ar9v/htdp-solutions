#lang htdp/isl+

(require 2htdp/image)

;;; Design the function `add-savannah`. The function consumes an image and four numbers:
;;;
;;; 1. the x-coordinate of a line's base point
;;; 2. the y-coordinate of a line's base point
;;; 3. the length of the line, and
;;; 4. the angle of the line
;;;
;;; It adds a fractal Savannah tree to the given image.
;;;
;;; Unless the line is too short, the function adds the specified line to the image.
;;; It then divides the line into three segments. It recursively uses the two intermediate
;;; points as the new starting points for two lines. The lengths and the angles of the
;;; two branches change in a fixed manner, but independently of each other. Use constants
;;; to define these changes and work with them until you like your tree well enough.

(define ε 10)

(define L-START #i0.33) ; where the new branches start in the original branch
(define R-START #i0.66)
(define L-FRACTION (- 1 #i0.33))
(define R-FRACTION (- 1 #i0.2))
(define LDELTA (/ 20 360))
(define RDELTA (- (/ 20 360)))

(define MT (empty-scene 500 500))

; add-savannah: Image N N N N -> Image
; generative: Constructs a tree by adding a branch of length `len` in `x`, `y` and then
; producing two new branches at angles LDELTA and RDELTA with new (shorter) lengths given
; by L-FRACTION and R-FRACTION.
;
; Termination: When the branch has a short-enough length, the scene is rendered as is.
;
; θ: The degrees expressed as a factor (e.g. 120/360), counter-clockwise
(define (add-savannah scene x y len θ)
  (cond [(too-small? len) scene]
        [else
         (local [(define scene1 (add-branch scene x y len θ))
                 (define center (make-posn x y))
                 (define lp (circle-pt center (* len L-START) θ))
                 (define lx (posn-x lp))
                 (define ly (posn-y lp))
                 (define rp (circle-pt center (* len R-START) θ))
                 (define rx (posn-x rp))
                 (define ry (posn-y rp))
                 (define scene2 (add-savannah scene1 lx ly (* len L-FRACTION) (+ θ LDELTA)))]
           (add-savannah scene2 rx ry (* len R-FRACTION) (+ θ RDELTA)))]))

; add-branch: Image N N N N -> Image
; Renders a branch of length `len` with a base at (x, y), and at an angle θ on scene.
(check-expect (add-branch MT 100 100 20 90/360)
              (scene+line MT 100 100 100 80 'black))
(check-expect (add-branch MT 100 150 20 45/360)
              (scene+line MT
                          100 150
                          (+ (* 20 (cos (* 45/360 2 pi))) 100)
                          (+ (* -20 (sin (* 45/360 2 pi))) 150)
                          'black))
(define (add-branch scene x y len θ)
  (local [(define p1 (circle-pt (make-posn x y) len θ))
          (define x1 (posn-x p1))
          (define y1 (posn-y p1))]
    (scene+line scene
                x y
                x1 y1
                'black)))

; too-small?: N -> Boolean
; Is `len` too small?
(define (too-small? len)
  (<= len ε))

; circle-pt: Number Number Number -> Posn
; determines the point on the circle with `center` and radius `r` whose angle is `factor`
;
; examples
; what are the x and y coordinates of the desired
; point, when given: 120/360, 240/360, 360/360
(define CENTER (make-posn 200 200))
(define RADIUS 200) ; the radius in pixels
(check-within (circle-pt CENTER RADIUS 120/360)
              (make-posn
               (+ (real-part (make-polar RADIUS (* 120/360 2 pi))) (posn-x CENTER))
               (+ (- (imag-part (make-polar RADIUS (* 120/360 2 pi)))) (posn-y CENTER)))
              0.001)
(define (circle-pt center r θ)
  (local [(define polar (make-polar r (* 2 pi θ)))
          (define x (+ (real-part polar) (posn-x center)))
          ; Unlike the previous exercise, we flip the y-coordinate here to make this
          ; behave like the cartesian plane we're used to
          (define y (+ (- (imag-part polar)) (posn-y center)))]
    (make-posn x y)))

(add-savannah MT 250 450 150 90/360)
