#lang htdp/bsl+

(require 2htdp/batch-io)

;;; Design `count-by-letter`. The function consumes a Dictionary and counts how often each
;;; letter is used as the first one of a word in the given dictionary. Its result is a list
;;; of Letter-Counts, a piece of data that combines letters and counts.
;;;
;;; Once your function is designed, determine how many words appear for all letters in your
;;; computer's dictionary.

(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings:
; -- "a"
; -- ...
; -- "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

(define-struct letter-count [letter tally])
; A LetterCount is a structure
;   (make-letter-count Letter Number)
;
; interpretation: (make-letter-count l n) represents a tally of `l`

(define TEST-DICT
  (list "abnormal" "account" "atrophy"
        "banal" "birch" "bounty"
        "church" "canal"
        "daunting" "denotational"
        "easy" "ecology" "ephemeral" "eulogy"
        "face" "fibula" "fulcrum"
        "zen" "zygomatic"))

; starts-with#: Letter Dictionary -> Number
; Counts how many words in `dict` start with letter `l`
(check-expect (starts-with# "a" TEST-DICT) 3)
(check-expect (starts-with# "c" TEST-DICT) 2)
(check-expect (starts-with# "e" TEST-DICT) 4)
(check-expect (starts-with# "k" TEST-DICT) 0)
(define (starts-with# l dict)
  (cond [(empty? dict) 0]
        [(cons? dict)
         (if (string=? l (string-ith (first dict) 0))
             (+ 1 (starts-with# l (rest dict)))
             (starts-with# l (rest dict)))]))

; count-by-letter: Dictionary -> [LetterCount]
; Counts how many words in `dict` are started by each letter in the alphabet
(check-expect (count-by-letter '())
              (list (make-letter-count "a" 0)
                    (make-letter-count "b" 0)
                    (make-letter-count "c" 0)
                    (make-letter-count "d" 0)
                    (make-letter-count "e" 0)
                    (make-letter-count "f" 0)
                    (make-letter-count "g" 0)
                    (make-letter-count "h" 0)
                    (make-letter-count "i" 0)
                    (make-letter-count "j" 0)
                    (make-letter-count "k" 0)
                    (make-letter-count "l" 0)
                    (make-letter-count "m" 0)
                    (make-letter-count "n" 0)
                    (make-letter-count "o" 0)
                    (make-letter-count "p" 0)
                    (make-letter-count "q" 0)
                    (make-letter-count "r" 0)
                    (make-letter-count "s" 0)
                    (make-letter-count "t" 0)
                    (make-letter-count "u" 0)
                    (make-letter-count "v" 0)
                    (make-letter-count "w" 0)
                    (make-letter-count "x" 0)
                    (make-letter-count "y" 0)
                    (make-letter-count "z" 0)))
(check-expect (count-by-letter TEST-DICT)
              (list (make-letter-count "a" 3)
                    (make-letter-count "b" 3)
                    (make-letter-count "c" 2)
                    (make-letter-count "d" 2)
                    (make-letter-count "e" 4)
                    (make-letter-count "f" 3)
                    (make-letter-count "g" 0)
                    (make-letter-count "h" 0)
                    (make-letter-count "i" 0)
                    (make-letter-count "j" 0)
                    (make-letter-count "k" 0)
                    (make-letter-count "l" 0)
                    (make-letter-count "m" 0)
                    (make-letter-count "n" 0)
                    (make-letter-count "o" 0)
                    (make-letter-count "p" 0)
                    (make-letter-count "q" 0)
                    (make-letter-count "r" 0)
                    (make-letter-count "s" 0)
                    (make-letter-count "t" 0)
                    (make-letter-count "u" 0)
                    (make-letter-count "v" 0)
                    (make-letter-count "w" 0)
                    (make-letter-count "x" 0)
                    (make-letter-count "y" 0)
                    (make-letter-count "z" 2)))
(define (count-by-letter dict)
  (count-by-letters LETTERS dict))

; count-by-letters: [Letter] Dictionary -> [LetterCount]
; Counts how many words in `dict` are started by each letter in `letters`
(check-expect (count-by-letters '() '()) '())
(check-expect (count-by-letters (list "a" "b") '())
              (list (make-letter-count "a" 0)
                    (make-letter-count "b" 0)))
(check-expect (count-by-letters (list "a" "b" "c" "d" "e" "f" "z") TEST-DICT)
              (list (make-letter-count "a" 3)
                    (make-letter-count "b" 3)
                    (make-letter-count "c" 2)
                    (make-letter-count "d" 2)
                    (make-letter-count "e" 4)
                    (make-letter-count "f" 3)
                    (make-letter-count "z" 2)))
(define (count-by-letters letters dict)
  (cond [(empty? letters) '()]
        [(cons? letters)
         (cons (make-letter-count (first letters) (starts-with# (first letters) dict))
               (count-by-letters (rest letters) dict))]))

;; (count-by-letter AS-LIST)
