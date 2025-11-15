#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define MTS (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))

; A Posn represents the state of the world

; main: Posn -> Posn
(define (main p0)
  (big-bang p0
            [on-tick x+]
            [on-mouse reset-dot]
            [to-draw scene+dot]))

; scene+dot: Posn -> Image
; adds a red spot to MTS at p
(check-expect (scene+dot (make-posn 10 20)) (place-image DOT 10 20 MTS))
(check-expect (scene+dot (make-posn 88 73)) (place-image DOT 88 73 MTS))
(define (scene+dot p)
  (place-image DOT (posn-x p) (posn-y p) MTS))

; x+: Posn -> Posn
; increses the x-coordinate of p by 3
(define (x+ p)
  (make-posn (+ (posn-x p) 3) (posn-y p)))

; reset-dot: Posn Number Number MouseEvent -> Posn
; for mouse clicks, `(make-posn x y)`; otherwise `p`
(check-expect
 (reset-dot (make-posn 10 20) 29 31 "button-down")
 (make-posn 29 31))

(check-expect
 (reset-dot (make-posn 10 20) 29 31 "button-up")
 (make-posn 10 20))
(define (reset-dot p x y me)
  (cond [(mouse=? "button-down" me) (make-posn x y)]
        [else p]))
