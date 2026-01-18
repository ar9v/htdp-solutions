#lang htdp/isl+

;;; Use `map` to define the function `convert-euro`, which converts a list of US$ amounts
;;; into a list of € amounts based on an exchange rate of US$1.06 per €.

;;; Also use map to define `convertFC`, which converts a list of Fahrenheit measurements
;;; to al list of Celsius measurements

;;; Finally, try hour hand at `translate`, a function that translates a list of Posns
;;; into a list of lists of pairs of numbers

(define EURO-TO-DOLLARS-RATE 106) ; in cents

; convert-euro: [List-of Number] -> [List-of Number]
; converts a list of of USD (in cents) to Euros (in cents)
(check-expect (convert-euro '()) '())
(check-expect (convert-euro (list 100 200))
              (list (/ 100 EURO-TO-DOLLARS-RATE) (/ 200 EURO-TO-DOLLARS-RATE)))
(define (convert-euro usds)
  (map (λ (usd) (/ usd EURO-TO-DOLLARS-RATE)) usds))

; convertFC: [List-of Fahrenheit] -> [List-of Celsius]
; converts a list of Fahrenheit measurements into a list of Celsius ones
(check-expect (convertFC '()) '())
(check-expect (convertFC '(212 32 44))
              (list ((λ (f) (* 5/9 (- f 32))) 212)
                    ((λ (f) (* 5/9 (- f 32))) 32)
                    ((λ (f) (* 5/9 (- f 32))) 44)))
(define (convertFC lof)
  (map (λ (f) (* 5/9 (- f 32))) lof))

; translate: [List-of Posn] -> [List-of [List-of Number]]
; Turns `posns` into a list of lists, each of which is a pair of numbers (the coordinates)
(check-expect (translate '()) '())
(check-expect (translate (list (make-posn 1 2) (make-posn -4 -6)))
              '((1 2) (-4 -6)))
(define (translate posns)
  (map (λ (p) (list (posn-x p) (posn-y p))) posns))
