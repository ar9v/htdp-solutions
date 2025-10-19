#lang htdp/bsl

;; After studying the costs of a show, the owner discovered several ways of lowering
;; the cost. As a result of these improvements, there is no longer a fixed cost; a
;; variable cost of $1.50 per attendee remains.
;;
;; Modify both programs to reflect this change. When the programs are modified, test
;; them again with ticket prices of $3, $4, and $5 and compare the results.

;;; Program 1
(define BASE-PRICE 5.0)
(define ATTENDEES-AT-BASE-PRICE 120)
(define RATE-OF-ATTENDEE-CHANGE (/ 15 0.1))

;;; Change 1
;; (define FIXED-COST 180)                 ; We can comment this line out

;;; Change 2
;; (define RATE-OF-COST-CHANGE 0.04)
(define RATE-OF-COST-CHANGE 1.5)

(define (attendees ticket-price)
  (- ATTENDEES-AT-BASE-PRICE (* (- ticket-price BASE-PRICE) RATE-OF-ATTENDEE-CHANGE)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  ;; Change 3
  ;; (+ (* RATE-OF-COST-CHANGE (attendees ticket-price)))
  (* RATE-OF-COST-CHANGE (attendees ticket-price)))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

;;; Program 2
(define (inlined-profit price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
     ;; (+ 180
     ;;    (* 0.04
     ;;       (+ 120
     ;;          (* (/ 15 0.1)
     ;;             (- 5.0 price)))))
     ;;
     ;; All changes have to be made here.
     (* 1.5
        (+ 120
           (* (/ 15 0.1)
              (- 5.0 price))))))

;;; Tests
(= (inlined-profit 3) (profit 3))
(= (inlined-profit 4) (profit 4))
(= (inlined-profit 5) (profit 5))
