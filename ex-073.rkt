#lang htdp/bsl

;;; Design the function `posn-up-x`, which consumes a Posn `p` and a Number `n`. It
;;; produces a Posn like `p` with `n` in the `x` field.
;;;
;;; A neat observation is that we can define `x+` using `posn-up-x`:
;;; (define (x+ p)
;;;   (posn-up-x p (+ (posn-x p) 3)))

; Keeping this definition to illustrate the relationship between the functions via
; a test
;
; x+: Posn -> Posn
; increses the x-coordinate of p by 3
(define (x+ p)
  (make-posn (+ (posn-x p) 3) (posn-y p)))

; posn-up-x: Posn Number -> Posn
; Given Posn `p` and a Number `n`, returns a new Posn where the `x` field is `n`
(check-expect (posn-up-x (make-posn 0 0) 1) (make-posn 1 0))
(check-expect (posn-up-x (make-posn -3 2) 9) (make-posn 9 2))
(check-expect (posn-up-x (make-posn 5 6) 8) (x+ (make-posn 5 6)))
(define (posn-up-x p n)
  (make-posn n (posn-y p)))
