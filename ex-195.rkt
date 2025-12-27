#lang htdp/bsl+

(require 2htdp/batch-io)

;;; Design the function `starts-with#`, which consumes a Letter and Dictionary and then
;;; counts how many words in the given Dictionary start with the given Letter. Once you
;;; know that your function works, determine how many words start with "e" in your
;;; computer's dictionary and how many with "z".

(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings:
; -- "a"
; -- ...
; -- "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

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

(starts-with# "e" AS-LIST)
(starts-with# "z" AS-LIST)
