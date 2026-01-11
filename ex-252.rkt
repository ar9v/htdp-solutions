#lang htdp/isl

(require 2htdp/image)

;;; Design `fold2`, which is the abstraction of the two functions in figure 94. Compare
;;; this exercise with exercise 251. Even though both involve the `product` function, this
;;; exercise poses an additional challenge because the second function, `image*`, consumes
;;; a list of Posns and produces an Image. Still, the solution is within reach of the
;;; material in this section, and it is especially worth comparing the solution with the
;;; one to the preceding exercise. The comparison yields interesting insights into
;;; abstract signatures.

; place-dot: Posn Image -> Image
(define (place-dot p img)
  (place-image
   dot
   (posn-x p) (posn-y p)
   img))

; graphical constants:
(define emt
  (empty-scene 100 100))

(define dot
  (circle 3 "solid" "red"))

; product: [List-of Number] -> Number
; fold1:   (Number Number -> Number) Number [List-of Number] -> Number

; image*: [List-of Posn] -> Image
; To "fold" it:
; -- (_ _ -> _) _ [List-of _] -> _
; -- (_ _ -> _) Image [List-of Posn] -> Image      ; we get these from `image*`'s signature
; -- (_ _ -> Image) Image [List-of Posn] -> Image  ; has to match the return value
; -- (_ Image -> Image) Image [List-of Posn] -> Image  ; Since that's the "rest", so ...
;
;
; -- (Posn Image -> Image) Image [List-of Posn] -> Image
; -- (   X    Y  ->     Y)     Y [List-of    X] -> Y

; fold2: [X Y -> Y] Y [List-of X] -> Y
; Produces the result of applying `f` to successive elements of `l`, from right to left.
(check-expect (fold2 * 1 (list 1 2 3)) 6)
(check-expect (fold2 place-dot emt (list (make-posn 10 10)
                                         (make-posn 40 40)
                                         (make-posn 80 80)))
              (place-image dot
                           10 10
                           (place-image dot
                                        40 40
                                        (place-image dot
                                                     80 80
                                                     emt))))
(define (fold2 f n0 l)
  (cond [(empty? l) n0]
        [(cons? l) (f (first l) (fold2 f n0 (rest l)))]))


;;; Both versions are written the same way. The insight is realizing that, in our
;;; signature, X and Y happen to be the same type for functions like `+` and `*`, but
;;; can be different for other cases, like `image*`, where X is Posn and Y is Image.
