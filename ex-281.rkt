#lang htdp/isl+

(require 2htdp/image)

;;; Write down a lambda expression that

(define-struct ir [name price])

; 1. consumes a number and decides whether it is less than 10;
(lambda (n) (< n 10))

; 2. multiplies two given numbers and turns the result into a string;
(lambda (x y) (number->string (* x y)))

; 3. consumes a natural number and returns 0 for evens and 1 for odds;
(lambda (n) (if (even? n) 0 1))

; 4. consumes two inventory records and compares them by price; and
(lambda (ir1 ir2) (< (ir-price ir1) (ir-price ir2)))

; 5. adds a red dot at a given Posn to a given Image.
(lambda (p img)
  (place-image (circle 3 "solid" "red")
               (posn-x p) (posn-y p)
               img))
