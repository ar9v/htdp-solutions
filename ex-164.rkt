#lang htdp/bsl

;;; Design the function `convert-euro`, which converts a list of US$ amounts into a list of
;;; € amounts. Look up the current exchange rate on the web.
;;;
;;; Generalize `convert-euro` to the function `convert-euro*`, which consumes an exchange
;;; rate and a list of US$ amounts and converts the latter into a list of € amounts.

;; Exchange Rate: 1 USD = 0.85 EUR
(define CURRENT-EXCHANGE-RATE 0.85)

; convert-euro: Number USD -> EUR
; Converts `usd` to its Euro value given an `exchange-rate`
(check-expect (convert-euro CURRENT-EXCHANGE-RATE 1) CURRENT-EXCHANGE-RATE)
(check-expect (convert-euro CURRENT-EXCHANGE-RATE 25) (* 25 CURRENT-EXCHANGE-RATE))
(define (convert-euro exchange-rate usd)
  (* usd exchange-rate))

; convert-euro*: List-of-USD Number -> List-of-EUR
; Converts all `usds` amounts into Euro equivalents given an `exchange-rate`
(check-expect (convert-euro* CURRENT-EXCHANGE-RATE '()) '())
(check-expect (convert-euro* CURRENT-EXCHANGE-RATE (cons 25 (cons 1 '())))
              (cons (convert-euro CURRENT-EXCHANGE-RATE 25)
                    (cons (convert-euro CURRENT-EXCHANGE-RATE 1)
                          '())))
(define (convert-euro* exchange-rate usds)
  (cond [(empty? usds) '()]
        [(cons? usds)
         (cons (convert-euro exchange-rate (first usds))
               (convert-euro* exchange-rate (rest usds)))]))
