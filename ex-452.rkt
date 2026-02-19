#lang htdp/isl+

;;; Both `first-line` and `remove-first-line` are missing purpose statements. Articulate
;;; proper statements.

(define NEWLINE "\n") ; the 1String

; File -> Line
; Returns `afile`'s first line, i.e. a list with all the characters from the start
; and up to the first newline (excluding the newline)
(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) '()]
    [else (cons (first afile) (first-line (rest afile)))]))

; File -> File
; Returns a File like `afile`, but without the first line, i.e. it returns a list of
; all characters that come after the first newline, if any.
(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) (rest afile)]
    [else (remove-first-line (rest afile))]))
