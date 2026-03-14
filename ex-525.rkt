#lang htdp/isl+

(require 2htdp/image)

;;; Tackle the wish list that the skeleton implies

(define ε 10)
(define SCENE (empty-scene 100 100))

; add-triangle: Image Posn Posn Posn -> Image
; adds the black traingle a, b, c to scene
;
; assumption: a, b, c form a triangle, e.g. they don't lie on the same line
(check-expect (add-triangle SCENE (make-posn 5 20) (make-posn 10 10) (make-posn 15 20))
              (scene+line (scene+line (scene+line SCENE 5 20 10 10 'black)
                                      10 10 15 20 'black)
                          15 20 5 20 'black))
(define (add-triangle scene a b c)
  (local [(define ax (posn-x a))
          (define ay (posn-y a))
          (define bx (posn-x b))
          (define by (posn-y b))
          (define cx (posn-x c))
          (define cy (posn-y c))]
    (scene+line (scene+line (scene+line scene ax ay bx by 'black)
                            bx by cx cy 'black)
                cx cy ax ay 'black)))

; too-small?: Posn Posn Posn -> Boolean
; is the triangle a, b, c too small to be divided
;
; assumption: a, b, c form an equilateral triangle
(check-expect (too-small? (make-posn 0 0) (make-posn 1 1) (make-posn 2 0)) #true)
(define (too-small? a b c)
  (<= (distance a b) ε))

; distance: Posn Posn -> Number
; Returns the distance between a and b
(check-expect (distance (make-posn 0 0) (make-posn 5 0)) 5)
(check-within (distance (make-posn 5 10) (make-posn 7 20))
              (sqrt (+ (sqr (- 5 7))
                       (sqr (- 10 20))))
              0.001)
(define (distance a b)
  (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
           (sqr (- (posn-y a) (posn-y b))))))

; mid-point: Posn Posn -> Posn
; determines the midpoint between a and b
(check-expect (mid-point (make-posn 5 6) (make-posn 8 9))
              (make-posn (/ (+ 5 8) 2) (/ (+ 6 9) 2)))
(define (mid-point a b)
  (make-posn (/ (+ (posn-x a) (posn-x b)) 2)
             (/ (+ (posn-y a) (posn-y b)) 2)))
