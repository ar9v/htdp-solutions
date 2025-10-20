#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

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

;;; 2.5 Programs
(require 2htdp/batch-io)

(write-file "sample.dat" "212")
(read-file "sample.dat")

(define (C f)
  (* 5/9 (- f 32)))

(define (convert in out)
  (write-file out
              (string-append
               (number->string (C (string->number (read-file in))))
               "\n")))

;;; Explain `(string-append ... "\n")`
;;;
;;; A: This will simply add a new line to `out`.

(convert "sample.dat" 'stdout)
(convert "sample.dat" "out.dat")
(read-file "out.dat")

(define (number->square s)
  (square s "solid" "red"))

(define (reset s ke) 100)

;;; Stop! Explain what happens when you hit "return", count to 10, and finally press
;;; "a".
;;; (context:
;;; (big-bang 100
;;;           [to-draw number->square]
;;;           [on-tick sub1]
;;;           [stop-when zero?]
;;;           [on-key reset])
;;;
;;; A: Per page 17 (in the Prologue), the clock tick 28 times per second, which means that
;;;    by the time we've counted to 10 (assuming we're counting a number per second),
;;;    the program has already processed 280 ticks.
;;;
;;;    So, `big-bang`  will have returned by then! We need to press a key before before
;;;    within ~3.5 seconds. Then, the square will reinflate to full size and begin to
;;;    shrink again.

;;; (define cw1 (ke-h cw0 "a"))
;;; (define cw2 (tock cw1))
;;; (define cw3 (me-h cw2 "button-down" 90 100))
;;;
;;; Stop! How does `big-bang` display each of these three states?
;;; A: By using `render` after each event has been processed.
;;;
;;;
;;; Stop! Reformulate the first sequence of events as an expression.
;;;
;;; A: (me-h (tock (ke-h cw0 "a")) "button-down" 90 100)
(define BACKGROUND (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))

(define (main y)
  (big-bang y
            [on-tick sub1]
            [stop-when zero?]
            [to-draw place-dot-at]
            [on-key stop]))

(define (place-dot-at y) (place-image DOT 50 y BACKGROUND))
(define (stop y ke) 0)

;;; Stop! Try now to understand how main reacts when you press a key. One way to find
;;; out whether your conjecture is correct is to launch the `main` function on some
;;; reasonable number (e.g. `(main 90)`)
;;;
;;; A: Main receives a key event, which in turn is passed on to our registered handler,
;;;    `stop`. Stop returns 0, so `big-bang` uses that as the next state. Then, `stop-when`
;;;    is evaluated as a part of the next cycle, which evaluates to `#true`. `render` is
;;;    not called, so (1) `big-bang` returns 0 (the last state) and (2) the rendered image
;;;    is that of a dot at the last state before it was set to 0 by `stop`.
