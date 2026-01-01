#lang htdp/bsl+

; A Word is one of
; -- '()
; -- (cons 1String Word)
;
; interpretation a Word is a list of 1Strings (letters)

; A List-of-words is ...

; Word -> List-of-words
; finds all rearrangements of `word`
; (define (arrangements word)
;   (list word))

;;; The above leaves us with two additional wishes: a function that consumes a String and
;;; produces its corresponding Word, and a function for the opposite direction.
;;;
;;; Here are the wish-list entries:

; string->word: String -> Word
; convert `s` to the chosen Word representation
(check-expect (string->word "") '())
(check-expect (string->word "act") (list "a" "c" "t"))
(define (string->word s)
  (explode s))

; word->string: Word -> String
; converts `w` into a String
(check-expect (word->string '()) "")
(check-expect (word->string (list "a" "c" "t")) "act")
(define (word->string w)
  (implode w))

;;; Look up the data definition for Word in the next section and complete the definitions
;;; of `string->word` and `word->string`
;;;
;;; HINT: You may wish to look in the list of functions that BSL provides.
