#lang htdp/isl+

(require 2htdp/abstraction)
(require 2htdp/batch-io)

;;; Design the function `words-on-line`, which determines the number of Strings per item in
;;; a list of list of strings

; words-on-line: [List-of [List-of String]] -> [List-of Number]
; Determines the number of Strings per item in `lines`
(check-expect (words-on-line (read-words/line "ttt.txt")) '(1 0 5 5 3 1 0 5 3 5 3 0 2))
(define (words-on-line lines)
  (for/list [(line lines)] (length line)))
