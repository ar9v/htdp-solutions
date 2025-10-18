#lang htdp/bsl

;; Our solution to the sample problem (Ch. 2.3) contains several constants in the middle
;; of functions. As "One Program, Many Definitions" already points out, it is best to
;; give names to such constants so that future readers understand where these numbers
;; come from. Collect all definitions in DrRacket's definitions area and change them
;; so that all magic numbers are refactored into constant definitions.

(define BASE-PRICE 5.0)
(define ATTENDEES-AT-BASE-PRICE 120)
(define RATE-OF-ATTENDEE-CHANGE (/ 15 0.1))
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
