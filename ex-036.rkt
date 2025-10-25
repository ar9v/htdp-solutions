#lang htdp/bsl

(require 2htdp/image)

;; Design the function `image-area`, which counts the number of pixels in a given image.

;;; A:

;;; 1. Data Definitions
;;;
;;; Nothing much to do here, since images are primitives.

;;; 2. Signature, Statement Of Purpose, Header
;;;
;;; image-area: Image -> Number
;;; Computes the area of an image, or the number of pixels in it.
;;;
;;; (define (image-area img) 1)

;;; 3. Functional Examples
;;;
;;; given: (square 3 "solid" "blue"), expect: 9
;;; given: (rectangle 2 3 "solid" "green"), expect: 6
;;; given: (circle 4 "solid" "brown"), expect: 64

;;; 4. Inventory/Function Template
;;;
;;; (define (image-area img) (... img ...))

;;; 5. Code/Put it together
;;;
;;; image-area: Image -> Number
;;; Computes the area of an image, or the number of pixels in it.
;;;
;;; given: (square 3 "solid" "blue"), expect: 9
;;; given: (rectangle 2 3 "solid" "green"), expect: 6
;;; given: (circle 4 "solid" "brown"), expect: 64
(define (image-area img)
  (* (image-width img) (image-height img)))

;;; 6. Tests
(and
 (= (image-area (square 3 "solid" "blue")) 9)
 (= (image-area (rectangle 2 3 "solid" "green")) 6)
 (= (image-area (circle 4 "solid" "brown")) 64))
