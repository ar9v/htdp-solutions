#lang htdp/bsl

;;; Evaluate the following program, step-by-step:

(define PRICE 5)
(define SALES-TAX (* 0.08 PRICE))
;; (define SALES-TAX (* 0.08 5))
;; ==
;; (define SALES-TAX 0.4)

(define TOTAL (+ PRICE SALES-TAX))
;; (define TOTAL (+ 5 0.4))
;; ==
;; (define TOTAL 5.4)

;;; Does the evaluation of the following program signal an error?
;; (define COLD-F 32)
;; (define COLD-C (fahrenheit->celsius COLD-F))
;; (define (fahrenheit->celsius f)
;;   (* 5/9 (- f 32)))

;;; A: It does! It calls fahrenheit->celsius before it is defined.


;;; How about the next one?
(define LEFT -100)
(define RIGHT 100)
(define (f x) (+ (* 5 (expt x 2)) 10))
(define f@left (f LEFT))
(define f@right (f RIGHT))

;;; A: This does not signal an error. `f` is defined before it is used (and the names,
;;; although funky, are legal)
