#lang htdp/bsl

;; Introduce constant definitions that separate the intervals
;; for low prices and luxury prices from the others so that the legislators in
;; Tax Land can easily raise the taxes even more

; A Price falls into one of three intervals:
; --- 0 through 1000
; --- 1000 through 10000
; --- 10000 and above.
; interpretation the price of an item

;; Constants
(define LOW-COST-MAX-PRICE 1000)
(define LUXURY-COST-MIN-PRICE 10000)

(define LOW-COST-RATE 0.05)
(define LUXURY-COST-RATE 0.08)

(check-expect (sales-tax 0) 0)
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax LOW-COST-MAX-PRICE) (* LOW-COST-RATE 1000))
(check-expect (sales-tax 5000) (* LOW-COST-RATE 5000))
(check-expect (sales-tax LUXURY-COST-MIN-PRICE) (* LUXURY-COST-RATE 10000))
(check-expect (sales-tax 12017) (* LUXURY-COST-RATE 12017))
(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p LOW-COST-MAX-PRICE)) 0]
    [(and (<= LOW-COST-MAX-PRICE p) (< p LUXURY-COST-MIN-PRICE)) (* LOW-COST-RATE p)]
    [(>= p LUXURY-COST-MIN-PRICE) (* LUXURY-COST-RATE p)]))
