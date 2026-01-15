#lang htdp/isl

(require 2htdp/batch-io)

;;; Chapter 12.1 deals with relatively simple tasks relating to English dictionaries. The
;;; design of two of them just call out for the use of existing abstractions:
;;;
;;; - Design `most-frequent`. The function consumes a Dictionary and produces the
;;;   Letter-Count for the letter that is most frequently used as the first one in the
;;;   words of the given dictionary.
;;;
;;; - Design `words-by-first-letter`. The function consumes a Dictionary and produces a
;;;   list of Dictionarys, one per Letter. Do *not* include '() if there are no words for
;;;   some letter; ignore the empty grouping instead.
;;;
;;; For the data definitions, see figure 74.

(define LOCATION "/usr/share/dict/words")
(define SYSTEM-DICTIONARY (read-lines LOCATION))

(define-struct letter-count [letter tally])
; A LetterCount is a structure
;   (make-letter-count Letter Number)
;
; interpretation: (make-letter-count l n) represents a tally of `l`

; A Dictionary is a [List-of String]
(define TEST-DICT
  (list "abnormal" "account" "atrophy"
        "banal" "birch" "bounty"
        "church" "canal"
        "daunting" "denotational"
        "easy" "ecology" "ephemeral" "eulogy"
        "face" "fibula" "fulcrum"
        "zen" "zygomatic"))

; An Association is a 2 element list: (list X Y)
(define assoc-list (list (list "a" 1) (list "b" 2) (list "c" 3)))

; string-first: String -> 1String
; Plucks out the first 1String from `s`. Returns the empty string if `s` is empty
(check-expect (string-first "") "")
(check-expect (string-first "abc") "a")
(define (string-first s)
  (if (string=? s "") "" (string-ith s 0)))

; max-by: [X -> Number] [NonEmptyList-of X] -> X
; Given a list of items, returns the (first) item for which `f(item)` yields the highest
; value.
(check-error (max-by identity '()))
(check-expect (max-by identity '(1 3 2)) 3)
(check-expect (max-by string-length '("Bob" "Zack" "Elsa")) "Zack")
(define (max-by f l)
  (if (empty? l)
      (error "Error: list provided to max-by cannot be empty")
      (local [(define (max-by-f x res) (if (< (f res) (f x)) x res))]
        (foldl max-by-f (first l) (rest l)))))

; group-by: [X -> Y] [List-of X] -> [List-of Association]
; Given a list of items, uses `f` to determine a key to which an item belongs, and returns
; the list of associations
(check-expect (group-by identity '()) '())
(check-expect (group-by car '(("a" 1) ("a" 2) ("b" 3)))
              (list (list "a" '(("a" 1) ("a" 2)))
                    (list "b" '(("b" 3)))))
(define (group-by f l)
  (local [(define (combine i res)
            (local [(define (updater val) (cons i val))]
              (assoc-update res (f i) updater (list i))))]
    (foldr combine '() l)))

; assoc-update: [List-of Association] X [Y -> Z] Z -> [List-of Association]
; Updates `assc` by running `f` on the value for its `key`; uses `fallback` if the key is
; not found
(check-expect (assoc-update '() "b" identity "not found")
              (list (list "b" "not found")))
(check-expect (assoc-update assoc-list "b" add1 1)
              (list (list "a" 1) (list "b" 3) (list "c" 3)))
(check-expect (assoc-update assoc-list "d" add1 1)
              (list (list "d" 1) (list "a" 1) (list "b" 2) (list "c" 3)))
(define (assoc-update assc key updater fallback)
  (local [(define val (assoc key assc))
          (define (combine x res)
            (cons (if (equal? (first x) key) (list key (updater (second x))) x)
                  res))]
    (if (false? val)
        (cons (list key fallback) assc)
        (foldr combine '() assc))))

; most-frequent: Dictionary -> Letter-Count
; Produces the LetterCount of the letter that is most frequently used as the first letter
; in the dictionary.
(check-expect (most-frequent TEST-DICT) (make-letter-count "e" 4))
(define (most-frequent d)
  (local [(define (pair->letter-count p) (make-letter-count (first p) (length (second p))))]
    (max-by letter-count-tally (map pair->letter-count (group-by string-first d)))))

; words-by-first-letter: Dictionary -> [Dictionary]
; Produces a list of dictionaries, i.e. a list of words. Each list contains words starting
; with each letter of the alphabet.
;
; Note: Since we use `group-by`, we (1) never get empty groups but (2) we also get all
; words that start with a capital letter (which apparently is a thing in that file)
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
  (map second (group-by string-first dict)))

(most-frequent SYSTEM-DICTIONARY)
