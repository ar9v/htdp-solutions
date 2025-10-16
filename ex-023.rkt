#lang htdp/bsl

;; The first 1String in "hello world" is "h". How does the following function compute this
;; result?
(define (string-first s)
  (substring s 0 1))

;; Use the stepper to confirm your ideas.
;;
;; The stepper doesn't give us more details other than:
;;
;; (string-first "hello world")
;; ===
;; (substring "hello world" 0 1)
;; ===
;; "h"
