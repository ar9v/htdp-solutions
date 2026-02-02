#lang htdp/isl+

(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

;;; Figure 133 mentions `read-xexpr/web`. See figure 132 for its signature and purpose
;;; statement and then read its documentation to determine the difference to its "plain"
;;; relative.
;;;
;;; A: The difference is `read-xexpr/web` will include whitespace when translating
;;;    the XML of the given URL, whereas the `plain` version will not.

;;; Figure 133 is also missing several important pieces, in particular the interpretation
;;; of `data` and purpose statements for all the locally defined functions. Formulate the
;;; missing pieces so that you get to understand the program.

(define PREFIX "https://www.google.com/finance?q=")
(define SIZE 22) ; font size
(define-struct data [price delta])

; A StockWorld is a structure: (make-data String String)
;
; interpretation: (make-data price delta) represents the current stock of a given company,
; where `price` is the current USD price of a stock, and `delta` represents how the
; price has changed with respect to the last valuation.

; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co))
          ; retrieve-stock-data: StockWorld -> StockWorld
          ; Translates the XML pointed to by `url` into a `StockWorld` by plucking out
          ; the "price" and "priceChange" meta values.
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))

          ; StockWorld -> Image
          ; Renders company `co`'s stock price
          (define (render-stock-data w)
            (local (; [StockWorld -> String] -> Image
                    ; creates a `text` image by applying struct selector `sel` to the
                    ; StockWorld state `w`, of size SIZE and color `col`
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text " " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
              [on-tick retrieve-stock-data 15]
              [to-draw render-stock-data])))

; get: Xexpr String -> String
; Given Xexpr `x` and an String `itemprop`, return the value for the `content` attribute
; of `x`'s content for which the "itemprop" value is `itemprop`
;
; Just a stub to get this to pass muster for this exercise (see ex. 386)
(define (get x itemprop)
  "15.00")
