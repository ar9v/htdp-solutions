#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `find-attr`. The function consumes a list of attributes and a symbol. If the
;;; attributes list associates the symbol with a string, the function retrieves this
;;; string; otherwise it returns #false. Look up `assq` and use it to define the function.

; find-attr: [List-of Attributes] Symbol -> [Maybe String]
; Given `attrs`, looks up the value for attribute `attr`, if it exists; returns #false
; if it doesn't exist.
(check-expect (find-attr 'attr '()) #false)
(check-expect (find-attr 'attr '((initial "red"))) #false)
(check-expect (find-attr 'initial '((initial "red"))) "red")
(check-expect (find-attr 'next '((state "red") (next "green"))) "green")
(define (find-attr attr attrs)
  (match (assq attr attrs)
    [(list _attr value) value]
    [#false #false]))
