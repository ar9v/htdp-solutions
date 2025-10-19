#lang htdp/bsl

;; Define constants for the price optimization program at the movie theater so that the
;; price sensitivity of attendance (15 people for every 10 cents) becomes a computed
;; constant.
;;
;; A: Had already kind of done that, in RATE-OF-ATTENDEE-CHANGE.
;;    It might be worthwhile making those numbers themselves constants, though.
(define BASE-PRICE 5.0)
(define ATTENDEES-AT-BASE-PRICE 120)
(define ATTENDEE-CHANGE 15)
(define PRICE-CHANGE 0.1)
(define RATE-OF-ATTENDEE-CHANGE (/ ATTENDEE-CHANGE PRICE-CHANGE))
(define FIXED-COST 180)
(define RATE-OF-COST-CHANGE 0.04)

(define (attendees ticket-price)
  (- ATTENDEES-AT-BASE-PRICE (* (- ticket-price BASE-PRICE) RATE-OF-ATTENDEE-CHANGE)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ FIXED-COST (* RATE-OF-COST-CHANGE (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))
