#lang htdp/bsl

;; 2.3
(define (letter fst lst signature-name)
  (string-append
   (opening fst)
   "\n\n"
   (body fst lst)
   "\n\n"
   (closing signature-name)))

(define (opening fst)
  (string-append "Dear " fst ","))

(define (body fst lst)
  (string-append
   "We have discovered that all people with the" "\n"
   "last name " lst " have won our lottery. So, " "\n"
   fst ", " "hurry and pick up your prize."))

(define (closing signature-name)
  (string-append
   "Sincerely,"
   "\n\n"
   signature-name
   "\n"))

;;; Sample Problem
;;; The owner of a monopolistic movie theater in a small town has complete freedom in
;;; setting ticket prices. Teh more he charges, the fewer people can afford tickets.
;;; The less he charges, the more it costs to run a show because attendance goes up.
;;; In a recent experiment the owner determined a relationship between the price of
;;; a ticket and average attendance.
;;;
;;; At a price of $5.00 per ticket, 120 people attend a performance. For each 10-cent
;;; change in ticket price, the average attendance changes by 15 people. That is, if the
;;; owner charges $5.10, some 105 people attend on the average; if the price goes down
;;; to $4.90, average attendance increases to 135. Let's translate this idea into a
;;; mathematical formula:
;;;
;;; avg. attendance = 120 people - ($(change in price) / $0.10) * 15 people

;;; Stop! Explain the minus sign before you proceed.
;;;
;;; A: The formula says that if the price increases, less people show up. A price increase
;;;    is the same as saying that `change in price` is positive, therefore resulting in a
;;;    subtraction.
;;;
;;;    Conversely, if the price decreases, that is the same as saying that
;;;    `change in price` is negative, which results in an addition, which tracks with the
;;;    intended meaning of the formula, which is that attendance increases.

;;; Unfortunately, the increased attendance also comes at an increased cost. Every
;;; performance comes at a fixed cost of $180 to the owner plus a variable cost of
;;; $0.04 per atendee.
;;;
;;; The owner would like to know the exact relationship between profit and ticket price
;;; in order to maximize the profit.

(define (attendees ticket-price)
  (- 120 (* (- ticket-price 5.0) (/ 15 0.1))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ 180 (* 0.04 (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))
