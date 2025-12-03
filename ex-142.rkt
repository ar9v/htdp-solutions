#lang htdp/bsl

(require 2htdp/image)

;;; Design the `ill-sized?` function, which consumes a list of images `loi` and a
;;; positive number `n`. It produces the first image on `loi` that is not an `n` by `n`
;;; square; if it cannot find such an image, it produces false

;;; Hint: Use this for the result part of the signature

(define N 10)

(define fitting
  (cons (square N "solid" "black")
        (cons (square N "solid" "green")
              (cons (square N "solid" "red") '()))))

(define ill-fitting
  (cons (square N "solid" "black")
        (cons (square (+ N 1) "solid" "green")
              (cons (circle N "solid" "red") '()))))

; ImageOrFalse is one of
; -- Image
; -- #false

; ill-sized?: List-of-images Number -> ImageOrFalse
; Produces the first image in `loi` to not conform to `n` x `n` dimensions. If all
; images conform, this returns #false.
(check-expect (ill-sized? '() N) #false)
(check-expect (ill-sized? fitting N) #false)
(check-expect (ill-sized? ill-fitting N) (square (+ N 1) "solid" "green"))
(define (ill-sized? loi n)
  (cond [(empty? loi) #false]
        [else
         (if (not (and (= n (image-width (first loi)))
                       (= n (image-height (first loi)))))
             (first loi)
             (ill-sized? (rest loi) n))]))
