#lang htdp/isl+

;;; Develop `n-inside-playground?`, a specification of the `random-posns` function below.
;;; The function generates a predicate that ensures that the length of the given list is
;;; some given count and that all Posns in this list are within a WIDTH by HEIGHT
;;; rectangle:

; distances in terms of pixels
(define WIDTH 300)
(define HEIGHT 300)

; random-posns: N -> [List-of Posn]
; generates n random Posns in [0, WIDTH) by [0, HEIGHT)
(check-satisfied (random-posns 3) (n-inside-playground? 3))
(define (random-posns n)
  (build-list
   n
   (λ (i) (make-posn (random WIDTH) (random HEIGHT)))))

(define (n-inside-playground? n)
  (λ (l)
    (and (= (length l) n)
         (andmap (λ (p) (and (<= 0 (posn-x p)) (< (posn-x p) WIDTH)
                             (<= 0 (posn-y p)) (< (posn-y p) HEIGHT)))
                 l))))

;;; Define `random-posns/bad` that satisfies `n-inside-playground?` and does not live up
;;; to the expectations implied by the above purpose statement
(define a-num (random 5))
(check-satisfied (random-posns/bad a-num) (n-inside-playground? a-num))
(define (random-posns/bad n)
  ; assumes WIDTH and HEIGHT are greater than 0
  (build-list n (λ (i) (make-posn (- WIDTH 1) (- HEIGHT 1)))))
