#lang htdp/bsl+

(require 2htdp/batch-io)

;;; Design `words-by-first-letter`. The function consumes
;;; a Dictionary and produces a list of Dictionarys, one per Letter.
;;;
;;; Redesign most-frequent from exercise 197 using this new function.
;;; Call the new function most-frequent.v2. Once you have completed
;;; the design, ensure that the two functions compute the same result on your
;;; computer’s dictionary:

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

; most-frequent.v2: Dictionary -> LetterCount
; Same as `most-frequent`, but implemented with words-by-first-letter
(check-expect (most-frequent.v2 TEST-DICT) (make-letter-count "e" 4))
(define (most-frequent.v2 dict)
  (dict-to-letter-count (max-dict (words-by-first-letter dict))))

; max-dict: NonEmptyList<Dictionary> -> Dictionary
; Picks out the longest dictionary from `dicts`
(define (max-dict dicts)
  (cond [(empty? (rest dicts)) (first dicts)]
        [else
         (dict> (first dicts) (max-dict (rest dicts)))]))

; dict>: Dictionary Dictionary -> Dictionary
; Picks out the longest Dictionary of the two. If they are the same length, it'll return
; the second dict.
(check-expect (dict> (list "a" "abs") (list "be")) (list "a" "abs"))
(check-expect (dict> (list "be") (list "a" "abs")) (list "a" "abs"))
(check-expect (dict> (list "a" "abs") (list "bark" "be")) (list "bark" "be"))
(define (dict> d1 d2)
  (if (> (length d1) (length d2)) d1 d2))

; dict-to-letter-count: NonEmptyDictionary -> LetterCount
; Given a Dictionary, turns it into a LetterCount
;
; assumption: `dict` contains words starting with the same letter
(check-expect (dict-to-letter-count (list "a" "am")) (make-letter-count "a" 2))
(check-expect (dict-to-letter-count (list "bark" "be" "blip")) (make-letter-count "b" 3))
(define (dict-to-letter-count dict)
  (make-letter-count
   (string-ith (first dict) 0)
   (length dict)))

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

; words-by-first-letter: Dictionary -> [Dictionary]
; Produces a list of dictionaries, i.e. a list of words. Each list contains words starting
; with each letter of the alphabet.
(check-expect (words-by-first-letter '()) '())
(check-expect (words-by-first-letter TEST-DICT)
              (list (list "abnormal" "account" "atrophy")
                    (list "banal" "birch" "bounty")
                    (list "church" "canal")
                    (list "daunting" "denotational")
                    (list "easy" "ecology" "ephemeral" "eulogy")
                    (list "face" "fibula" "fulcrum")
                    (list "zen" "zygomatic")))
(define (words-by-first-letter dict)
  (reject-empty (words-by-first-letter/all LETTERS dict)))

; words-by-first-letter/all: [Letter] Dictionary -> [Dictionary]
; Returns a list of dictionaries, each distinguished by the letter with which their words
; start.
(check-expect (words-by-first-letter/all '() '()) '())
(check-expect (words-by-first-letter/all '() TEST-DICT) '())
(check-expect (words-by-first-letter/all (list "a") TEST-DICT)
              (list (list "abnormal" "account" "atrophy")))
(check-expect (words-by-first-letter/all (list "a") '())
              (list '()))
(check-expect (words-by-first-letter/all (list "k") TEST-DICT)
              (list '()))
(define (words-by-first-letter/all letters dict)
  (cond [(empty? letters) '()]
        [(cons? letters)
         (cons (words-by-first-letter/letter (first letters) dict)
               (words-by-first-letter/all (rest letters) dict))]))

(check-expect (words-by-first-letter/letter "a" '()) '())
(check-expect (words-by-first-letter/letter "a" TEST-DICT)
              (list "abnormal" "account" "atrophy"))
(check-expect (words-by-first-letter/letter "k" TEST-DICT) '())
; words-by-first-letter/letter: Letter Dictionary -> Dictionary
; Returns a Dictionary with words from `dict` starting exclusively with `letter`
(define (words-by-first-letter/letter letter dict)
  (cond [(empty? dict) '()]
        [(cons? dict)
         (if (string=? letter (string-ith (first dict) 0))
             (cons (first dict) (words-by-first-letter/letter letter (rest dict)))
             (words-by-first-letter/letter letter (rest dict)))]))

; reject-empty: [Dictionary] -> [Dictionary]
; Returns a new list of dictionaries, with empty dictionaries pruned from `dicts`
(check-expect (reject-empty '()) '())
(check-expect (reject-empty
               (list (list "abs" "arc")
                     (list)
                     (list "car" "con")))
              (list (list "abs" "arc")
                    (list "car" "con")))
(define (reject-empty dicts)
  (cond [(empty? dicts) '()]
        [(cons? dicts)
         (if (empty? (first dicts))
             (reject-empty (rest dicts))
             (cons (first dicts) (reject-empty (rest dicts))))]))

; TODO: Exercises 195 through this one could
; -- be made more robust: a dictionary file may include capitalized words; dealing with
;    them allows for small perf gains w/o needing to leverage binary search
; -- be optimized: A bunch of these functions perform redundant work
;
; And the design alternatives noted in the book could be explored!
(check-expect
 (most-frequent AS-LIST)
 (most-frequent.v2 AS-LIST))
