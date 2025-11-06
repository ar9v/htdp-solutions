#lang htdp/bsl

(require 2htdp/image)

;; A `cond` expression is really just an expression and may therefore show up in the
;; middle of another expression:

;; (- 200 (cond [(> y 200) 0] [else y]))

;;; Use the stepper to evaluate the expression for y = 100 and y = 210

;;; y = 100
;; (- 200 (cond [(> 100 200) 0] [else 100]))
;; ==
;; (- 200 (cond [#false 0] [else 100]))
;; ==
;; (- 200 (cond [else 100]))
;; ==
;; (- 200 100)
;; 100
(check-expect (- 200 (cond [(> 100 200) 0] [else 100])) 100)

;;; y = 200
;; (- 200 (cond [(> 200 200) 0] [else 200]))
;; ==
;; (- 200 (cond [#false 0] [else 200]))
;; ==
;; (- 200 (cond [else 200]))
;; ==
;; (- 200 200)
;; 0
(check-expect (- 200 (cond [(> 200 200) 0] [else 200])) 0)


;;; Nesting `cond` expressions can eliminate common expressions. Consider the function
;;; for launching a rocket, repeated in figure 20:

(define WIDTH 100)
(define HEIGHT 60)
(define MTSCN (empty-scene WIDTH HEIGHT))
(define ROCKET (bitmap/file "images/rocket.png"))
(define ROCKET-CENTER-TO-TOP (- HEIGHT (/ (image-height ROCKET) 2)))

;; (define (create-rocket-scene.v5 h)
;;   (cond
;;     [(<= h ROCKET-CENTER-TO-TOP)
;;      (place-image ROCKET 50 h MTSCN)]
;;     [(> h ROCKET-CENTER-TO-TOP)
;;      (place-image ROCKET 50 ROCKET-CENTER-TO-TOP MTSCN)]))

;;; Both branches of the `cond` expression have the same shape except as indicated with
;;; ...
;;;
;;; (place-image ROCKET X ... MTSCN)

;;; Reformulate `create-rocket-scene.v5` to use a nested expression; the resulting
;;; function mentions `place-image` only once

(define (create-rocket-scene.v5 h)
  (place-image ROCKET
               50
               (cond [(<= h ROCKET-CENTER-TO-TOP) h]
                     [else ROCKET-CENTER-TO-TOP])
               MTSCN))
