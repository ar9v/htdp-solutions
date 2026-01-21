#lang htdp/isl+

(require 2htdp/abstraction)

;;; Use loops to define `convert-euro`. See exercise 267.

(define EURO-TO-DOLLARS-RATE 106) ; in cents

; convert-euro: [List-of Number] -> [List-of Number]
; converts a list of of USD (in cents) to Euros (in cents)
(check-expect (convert-euro '()) '())
(check-expect (convert-euro (list 100 200))
              (list (/ 100 EURO-TO-DOLLARS-RATE) (/ 200 EURO-TO-DOLLARS-RATE)))
(define (convert-euro usds)
  (for/list ([usd usds]) (/ usd EURO-TO-DOLLARS-RATE)))
