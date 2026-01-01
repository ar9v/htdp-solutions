#lang htdp/bsl+

;;; Write down the data definition for List-of-words.

; a List-of-words is
; -- '()
; -- (cons Word List-of-words)

;;; Make up examples of Words
(define cat (list "c" "a" "t"))
(define hat (list "h" "a" "t"))
(define nib (list "n" "i" "b"))

;;; and List-of-words
(define low (list cat nib hat))
