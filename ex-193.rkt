#lang htdp/bsl+

(require 2htdp/image)

;;; Here are two more ideas for defining `render-poly`:
;;;
;;; -- `render-poly` could `cons` the last item of `p` onto `p` and then call
;;;    `connect-dots`
;;;
;;; -- `render-poly` could add the first item of `p` to the end of `p` via a version of
;;;    `add-at-end` that works on Polygons.
;;;
;;; Use both ideas to define `render-poly`; make sure both definitions pass the test cases.


; An NELoP is one of:
; -- (cons Posn '())
; -- (cons Posn NELoP)

(define MT (empty-scene 50 50))

(define triangle-p
  (list (make-posn 20 10)
        (make-posn 20 20)
        (make-posn 30 20)))

(define square-p
  (list (make-posn 10 10)
        (make-posn 20 10)
        (make-posn 20 20)
        (make-posn 10 20)))

(define pentagon-p
  (list (make-posn 10 10)
        (make-posn 5 15)
        (make-posn 15 20)
        (make-posn 25 15)
        (make-posn 20 10)))

; render-poly: Image Polygon -> Image
; renders the given polygon `p` into `img`
(check-expect
 (render-poly MT triangle-p)
 (scene+line
  (scene+line
   (scene+line MT 20 10 20 20 "red")
   20 20 30 20 "red")
  30 20 20 10 "red"))
(check-expect
 (render-poly MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))
(define (render-poly img p)
  (render-line (connect-dots img p)
               (first p)
               (last p)))

; render-line: Image Posn Posn -> Image
; renders a line from p to q into img
(define (render-line img p q)
  (scene+line
   img
   (posn-x p) (posn-y p) (posn-x q) (posn-y q)
   "red"))

; connect-dots: Image NELoP -> Image
; connects the dots in `p` by rendering lines in `img`
(check-expect
 (connect-dots MT triangle-p)
 (scene+line
  (scene+line MT 20 20 30 20 "red")
  20 10 20 20 "red"))
(check-expect
 (connect-dots MT square-p)
 (scene+line
  (scene+line
   (scene+line MT 20 20 10 20 "red")
   20 10 20 20 "red")
  10 10 20 10 "red"))
(define (connect-dots img p)
  (cond [(empty? (rest p)) img]
        [else
         (render-line (connect-dots img (rest p))
                      (first p)
                      (second p))]))

; last: Polygon -> Posn
; extracts the last item from p
(check-expect (last triangle-p) (make-posn 30 20))
(check-expect (last square-p) (make-posn 10 20))
(check-expect (last pentagon-p) (make-posn 20 10))
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

; render-poly.vlast: Image Polygon -> Image
; renders the given polygon `p` into `img` (by consing the last Posn in `p` to itself)
(check-expect
 (render-poly.vlast MT triangle-p)
 (scene+line
  (scene+line
   (scene+line MT 20 10 20 20 "red")
   20 20 30 20 "red")
  30 20 20 10 "red"))
(check-expect
 (render-poly.vlast MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))
(define (render-poly.vlast img p)
  (connect-dots img (cons (last p) p)))


; render-poly.vfirst: Image Polygon -> Image
; renders the given polygon `p` into `img` (by appending the first Posn in `p` to itself)
(check-expect
 (render-poly.vfirst MT triangle-p)
 (scene+line
  (scene+line
   (scene+line MT 20 10 20 20 "red")
   20 20 30 20 "red")
  30 20 20 10 "red"))
(check-expect
 (render-poly.vfirst MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))
(define (render-poly.vfirst img p)
  (connect-dots img (append p (list (first p)))))
