#lang htdp/bsl

;; Determine the potential profit for these ticket prices: $1, $2, $3, $4 and $5.
;; Which prize maximizes the profit of the movie theater? Determine the best ticket
;; price to a dime.

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

;; (exact->inexact (profit 1))             ; 511.2
;; (exact->inexact (profit 2))             ; 937.2
;; (exact->inexact (profit 3))             ; 1063.2
;; (exact->inexact (profit 4))             ; 889.2
;; (exact->inexact (profit 5))             ; 415.2

;;; The latter seem to suggest that the maximum profit lies somewhere between $2 and $4
;;;
;;; If we try 3.1, we can quickly discard the range [3.1, 4]
;; (exact->inexact (profit 3.1))           ; 1059.3

;;; So we try every dime between 2 and 3 (inclusive)
(exact->inexact (profit 2.1))
(exact->inexact (profit 2.2))
(exact->inexact (profit 2.3))
(exact->inexact (profit 2.4))
(exact->inexact (profit 2.5))
(exact->inexact (profit 2.6))
(exact->inexact (profit 2.7))
(exact->inexact (profit 2.8))
(exact->inexact (profit 2.9))
(exact->inexact (profit 3.0))

;;; And find that $2.90 is the ideal ticket price
