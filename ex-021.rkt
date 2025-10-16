#lang htdp/bsl

(define (ff a)
  (* 10 a))

;; Use DrRacket's stepper to evaluate `(ff (ff 1))` step-by-step.
;;
;; (ff (ff 1))
;; ===
;; (ff (* 10 1))
;; ===
;; (ff 10)
;; ===
;; (* 10 10)
;; ===
;; 100
(ff (ff 1))

;; Also try `(+ (ff 1) (ff 1))`. Does DrRacket's stepper reuse the results of computations?
;;
;; (+ (ff 1) (ff 1))
;; ===
;; (+ (* 10 1) (ff 1))
;; ===
;; (+ 10 (ff 10))
;; ===
;; (+ 10 (* 10 1))
;; ===
;; (+ 10 10)
;; ===
;; 20
;;
;; The stepper does _not_ reuse the output of the first application.
(+ (ff 1) (ff 1))
