#lang htdp/bsl+

(require 2htdp/batch-io)

;;; Complete the design of `in-dictionary`, specified in figure 78.
;;;
;;; HINT: See chapter 12.1 for how to read a dictionary.

(define DICT-FILENAME "/usr/share/dict/words")
(define DICT (read-lines DICT-FILENAME))


; in-dictionary: [String] -> [String]
; picks out all those Strings that occur in the dictionary
(check-expect (in-dictionary '()) '())
(check-expect (in-dictionary (list "faloopsinarb" "wafagoo")) '())
(check-expect (in-dictionary (list "cat" "cta" "tca" "tac" "act" "atc"))
              (list "cat" "act"))
(define (in-dictionary words)
  (cond [(empty? words) '()]
        [(cons? words)
         (if (member? (first words) DICT)
             (cons (first words) (in-dictionary (rest words)))
             (in-dictionary (rest words)))]))
