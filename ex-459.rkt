#lang htdp/isl+
;;; Another simple integration method divides the area into many small rectangles. Each
;;; rectangle has a fixed width and is as tall as the function graph in the middle of the
;;; rectangle. Adding up the areas of the rectangles produces an estimate of the area under
;;; the functiong's graph.
;;;
;;; Using these rectangles, we can determine the area under the graph:
;;;
;;; W * f(a + 0W + S) + W * f(a + 1W + S) + W * f(a + 2W + S) ... W * f(a + (R - 1)W + S)
;;;
;;; Where
;;; - W = (b - a) / R
;;; - S = width / 2

;;; Turn the description into an ISL+ function. Adapt the test cases from figure 165 to
;;; this case.

(define ε 0.01)
(define R 159)

(define (constant x) 20)
(define (linear x) (* 2 x))
(define (square x) (* 3 (sqr x)))

; integrate: [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume a < b holds
(check-within (integrate constant 12 22) 200 ε)
(check-within (integrate linear 0 10) 100 ε)
(check-within (integrate square 0 10) (- (expt 10 3) (expt 0 3)) ε)
(define (integrate f a b)
  (local [(define W (/ (- b a) R))
          (define S (/ W 2))
          (define (integrate-rectangles i)
            (cond [(= i R) 0]
                  [else (+ (* W (f (+ a (* i W) S)))
                           (integrate-rectangles (add1 i)))]))]
    (integrate-rectangles 0)))

;;; The more rectangles the algorithm uses, the closer its estimate is to the
;;; actual area. Make `R` a top-level constant and increase it by factors of 10 until the
;;; algorithm's accuracy eliminates problems with an ε value of 0.1.

;;; Decrease ε to 0.01 and increase `R` enough to eliminate any failing test cases again.
;;; Compare the result to exercise 458.

; #i999.9901111506665 for R = 159
; #i937.5 for R = 2; closer, but R = 2 is too low to be a particularly reliable metric
(integrate square 0 10)
