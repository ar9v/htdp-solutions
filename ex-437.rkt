#lang htdp/isl+

;;; Define `solve` and `combine-solutions` so that
;;;
;;; -- `special` computes the length of its input
;;; -- `special` negates each number on the given list of numbers, and
;;; -- `special` uppercases the given list of strings
;;;
;;; What do you conclude from these exercises?

;; special: ...
;; (define (special P)
;;   (cond [(empty? P) (solve P)]
;;         [else
;;          (combine-solutions
;;           P
;;           (special (rest P)))]))


; len: [List-of X] -> Number
(define (len l)
  (local [(define (solve _trivial) 0)
          (define (combine-solutions _l sub-solution) (+ 1 sub-solution))]
    (cond [(empty? l) (solve l)]
          [else (combine-solutions l (len (rest l)))])))

; negate: [List-of Number] -> [List-of Number]
(define (negate alon)
  (local [(define (solve l) l)
          (define (combine-solutions alon sub-solution)
            (cons (- (first alon)) sub-solution))]
    (cond [(empty? alon) (solve alon)]
          [else (combine-solutions alon (negate (rest alon)))])))

; uppercase: [List-of String] -> [List-of String]
(define (uppercase los)
  (local [(define (solve l) l)
          (define (combine-solutions l sub-solution)
            (cons (string-upcase (first l)) sub-solution))]
    (cond [(empty? los) (solve los)]
          [else (combine-solutions los (uppercase (rest los)))])))


;;; A:
;;; We can conclude that structural recursion really is a special case of generative
;;; recursion.
