#lang htdp/bsl

(require 2htdp/image)

;; Take a look at this attempt to solve exercise 17
(define (image-classify img)
  (cond
    [(>= (image-height img) (image-width img)) "tall"]
    [(= (image-height img) (image-width img))  "square"]
    [(<= (image-height img) (image-width img)) "wide"]))

;; Does stepping through an application suggest a fix?
;;
;; Yes... but you have to give it a proper application to uncover the bug in the first
;; place! So, e.g. (image-classify (rectangle 2 10 "solid" "blue")) correctly returns
;; "tall".
(string=? "tall" (image-classify (rectangle 2 10 "solid" "blue")))

;; And (image-classify (rectangle 10 2 "solid" "blue")) will correctly return "wide"
(string=? "wide" (image-classify (rectangle 10 2 "solid" "blue")))

;; But trying to test for square images reveals the bug (!!)
(false?
 (string=? "square"
           (image-classify (square 5 "solid" "blue"))))

;; Even if you update the first `cond` branch to be `(< ...)`, stepping through a program
;; wouldn't reveal that the last `cond` branch's `=` (in `<=`) is redundant (!!)
