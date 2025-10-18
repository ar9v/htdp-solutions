#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; 1.6

;; Imagine (define x 0)
;; What do you think (if (= x 0) 0 (/ 1 x)) evaluates to in this context?
;;
;; Answer: 0
;; (if (= x 0) 0 (/ 1 x))
;; ===
;; (if (= 0 0) 0 (/ 1 x))
;; ===
;; (if #true 0 (/ 1 x))
;; ===
;; 0

;; In addition to `=`, BSL provides a host of other comparison primitives. Explain
;; what the following four comparison primitives determine about numbers:
;;
;; <:  Determines whether a number is less than another one. Generally, given n arguments,
;;     it determines whether it's true that they (strictly) increase.
;;
;; <=: Determines whether a number is less than or equal than another one.
;;     Generally, given n arguments, it determines whether they are monotonically
;;     increasing (i.e. they increase or at least _not_ decrease)
;;
;; >:  Determines whether a number is greater than another one. In the general case,
;;     it determines whether its arguments strictly decrease
;;
;; >=: Determines whether a number is greater than or equal to another one.
;;     In the general case, it determines whether its arguments are monotonically
;;     decreasing (i.e. they decrease or at least _not_ increase)
