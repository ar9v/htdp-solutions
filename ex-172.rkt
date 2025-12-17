#lang htdp/bsl

(require 2htdp/batch-io)

;;; Design the function `collapse`, which converts a list of lines into a string.
;;; The strings should be separated by blank spaces (" "). The lines should be separated
;;; with a newline ("\n").

(define line1 (cons "The" (cons "quick" (cons "brown" (cons "fox" '())))))
(define line2 (cons "jumps" (cons "over" (cons "the" (cons "lazy" (cons "dog" '()))))))

(define lls (cons line1 (cons line2 '())))

; collapse: List-of-lines -> String
; Turns a list of lines into a single string, where each line is separated by the newline
; character \n
(check-expect (collapse '()) "")
(check-expect (collapse lls) "The quick brown fox\njumps over the lazy dog")
(define (collapse lol)
  (cond [(empty? lol) ""]
        [(empty? (rest lol)) (join-line (first lol))]
        [(cons? lol)
         (string-append (join-line (first lol))
                        "\n"
                        (collapse (rest lol)))]))

;;; join-line: List-of-strings -> String
;;; Joins `los` into a single string, where each word is separated by a single space.
(check-expect (join-line line1) "The quick brown fox")
(check-expect (join-line line2) "jumps over the lazy dog")
(define (join-line los)
  (cond [(empty? los) ""]
        [(empty? (rest los)) (first los)]
        [(cons? los) (string-append (first los) " " (join-line (rest los)))]))

;;; Challenge:
;;; When you are finished, use the program like this
;;;
;;; (write-file "ttt.dat"
;;;             (collapse (read-words/line "ttt.txt")))
;;;
;;; To make sure the two files "ttt.dat" and "ttt.txt" are identical, remove all
;;; extraneous white spaces in your version of the T.T.T. poem.

(write-file "ttt.dat" (collapse (read-words/line "ttt.txt")))
