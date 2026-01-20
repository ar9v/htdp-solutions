#lang htdp/isl+

;;; Here is a simple ISL+ program

(define (p1 x y)
  (+ (* x y)
     (+ (* 2 x)
        (+ (* 2 y) 22))))

(define (p2 x)
  (+ (* 55 x) (+ x 11)))

(define (p3 x)
  (+ (p1 x 0)
     (+ (p1 x 1) (p2 x))))

;;; Draw arrows from `p1`'s `x` parameter to all its bound occurrences.

;; (define (p1  x  y)
;;         v----|
;;   (+ (* x y) |
;;              v
;;      (+ (* 2 x)
;;         (+ (* 2 y) 22))))

;;; Draw arrows from `p1` to all bound occurrences of `p1`. Check the results with
;;; DrRacket's CHECK SYNTAX functionality.

;; (define (p1 x y)
;; |--------|
;; |  (+ (* x y)
;; |     (+ (* 2 x)
;; |        (+ (* 2 y) 22))))
;; |
;; |  (define (p2 x)
;; |    (+ (* 55 x) (+ x 11)))
;; |
;; | (define (p3 x)
;; --------v
;; |  (+ (p1 x 0)
;; -----------v
;;        (+ (p1 x 1) (p2 x))))
