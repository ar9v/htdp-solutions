#lang htdp/bsl+

(require 2htdp/batch-io)

;;; Design `most-frequent`. The function consumes a Dictionary. It produces a LetterCount
;;; for the letter that occurs most often as the first one in the given Dictionary.
;;;
;;; What is the most frequently used letter in your computer's dictionary and how often
;;; is it used?
;;;
;;; A: "s", for 11,879 words, apparently.

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
(define (count-by-letter dict)
  (count-by-letters LETTERS dict))

; count-by-letters: [Letter] Dictionary -> [LetterCount]
; Counts how many words in `dict` are started by each letter in `letters`
(define (count-by-letters letters dict)
  (cond [(empty? letters) '()]
        [(cons? letters)
         (cons (make-letter-count (first letters) (starts-with# (first letters) dict))
               (count-by-letters (rest letters) dict))]))

; most-frequent: Dictionary -> LetterCount
; Given a `dict`, produces the LetterCount for which `count-by-letter` yields the highest
; tally.
(check-expect (most-frequent TEST-DICT) (make-letter-count "e" 4))
(define (most-frequent dict)
  (max-letter-count (count-by-letter dict)))

; max-letter-count: NonEmptyList<LetterCount> -> LetterCount
; Produces the LetterCount in `lcs` with the highest tally
(check-expect (max-letter-count (list (make-letter-count "a" 1)))
              (make-letter-count "a" 1))
(check-expect (max-letter-count (list (make-letter-count "a" 1)
                                      (make-letter-count "b" 3)
                                      (make-letter-count "c" 2)))
              (make-letter-count "b" 3))
(define (max-letter-count lcs)
  (cond [(empty? (rest lcs)) (first lcs)]
        [else (letter-count> (first lcs) (max-letter-count (rest lcs)))]))

; letter-count>: LetterCount LetterCount -> LetterCount
; Returns the LetterCount with the greates tally of the two
(define (letter-count> lc1 lc2)
  (if (> (letter-count-tally lc1) (letter-count-tally lc2))
      lc1 lc2))
