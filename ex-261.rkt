#lang htdp/isl

;;; Consider the function definition in figure 101. Both clauses in the nested `cond`
;;; expression extract the first item from `an-inv` and both compute
;;; `(extract (rest an-inv))`. Use `local` to name this expression. Does this help
;;; increase the speed at which the function computes its result? Significantly?
;;; A little bit? Not at all?

(define-struct ir [name price])
; An IR is a structure:
; (make-IR String Number)
; An Inventory is one of:
; -- '()
; -- (cons IR Inventory)

(define example-inventory
  (local ((define (create-ir price)
            (make-ir (string-append "Product-" (number->string (exact->inexact price)))
                     price)))
      (map create-ir (range 0 2 0.1))))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (cond
       [(<= (ir-price (first an-inv)) 1.0)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))])]))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1.v2 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local ((define item (first an-inv))
             (define rest-of-items (extract1.v2 (rest an-inv))))
         (cond
           [(<= (ir-price item) 1.0)
            (cons item rest-of-items)]
           [else rest-of-items]))]))

;;; Does not have any measurable effect (at least with `time`)
;;;
;;; This happens because picking out a field in a struct is quick, and because
;;; both branches will need the result of the recursion (put another way: we
;;; are not calling `(extract1 (rest an-inv))` more than once each cycle.
(time (extract1 example-inventory))
(time (extract1.v2 example-inventory))
