#lang htdp/bsl

;; Here is the definition of `==>`:
(define (==> x y)
  (or (not x) y))

;; Use the stepper to determine the value of (==> #true #false)
;;
;; (==> #true #false)
;; ===
;; (or (not #true) #false)
;; ===
;; (or #false #false)
;; ===
;; #false
