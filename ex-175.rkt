#lang htdp/bsl

(require 2htdp/batch-io)

;;; Design a BSL program that simulates the Unix command `wc`. The purpose of the command
;;; is to count the number of 1Strings, words, and lines in a given file. That is, the
;;; command consumes the name of a file and produces a value that consists of three
;;; numbers.

(define-struct wc-result [lines words characters])
; A WcResult is a structure
;   (make-wc-result Number Number Number)
;
; interpretation: (make-wc-result l w c) represents a tally of lines, words and characters.
;

; wc: String -> WcResult
; Computes the amount of characters, words and lines in a given file.
(check-expect (wc "ttt.txt") (make-wc-result 13 33 183))
(define (wc filename)
  (make-wc-result (length (read-lines filename))
                  (length (read-words filename))
                  (length (read-1strings filename))))
