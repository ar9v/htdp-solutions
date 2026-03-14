#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

;;; Implement the Bézier curve algorithm.

(define ε 10)

(define MT (empty-scene 800 800))

; bezier: Image Posn Posn Posn -> Image
; Draws a curved line from a to b using p as a perspective point
;
; generative: Establishes new perspective points by getting the midpoint of the lines
; that go from the perspective point to a and c. It then recurs using the new triangles
; generated from establishing the midpoint of both midpoints ma and mb.
;
; termination: stops when the line from a to b is small enough
(define (bezier scene a b p)
  (cond [(too-small? (distance a b))
         (scene+line scene (posn-x a) (posn-y a) (posn-x b) (posn-y b) 'black)]
        [else
         (local [(define ap (mid-point a p))
                 (define bp (mid-point b p))
                 (define abp (mid-point ap bp))
                 (define scene1 (bezier scene a abp ap))]
           (bezier scene1 abp b bp))]))

; too-small? N -> Boolean
; Is x too small?
(define (too-small? x)
  (< x ε))

; mid-point: Posn Posn -> Posn
; determines the midpoint between a and b
(check-expect (mid-point (make-posn 5 6) (make-posn 8 9))
              (make-posn (/ (+ 5 8) 2) (/ (+ 6 9) 2)))
(define (mid-point a b)
  (make-posn (/ (+ (posn-x a) (posn-x b)) 2)
             (/ (+ (posn-y a) (posn-y b)) 2)))


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

; bezier-sim: [List-of Posn] -> [List-of Posn]
(define PAD 10)
(define START-A (make-posn PAD (/ (image-height MT) 2)))
(define START-B (make-posn (- (image-width MT) PAD) (/ (image-height MT) 2)))
(define START (list START-A START-B (mid-point START-A START-B)))
(define (bezier-sim s0)
  (big-bang s0
            [to-draw (λ (s) (bezier MT (first s) (second s) (third s)))]
            [on-mouse
             (λ (s x y me)
               (cond [(mouse=? me "button-down")
                      (list (first s) (second s) (make-posn x y))]
                     [(mouse=? me "move")
                      (list (first s) (make-posn x y) (third s))]
                     [else s]))]))

(bezier-sim START)
