#lang htdp/bsl+

(require 2htdp/batch-io)

;;; Integrate `arrangements` with the partial program from chapter 12.3. After making
;;; sure that the entire suite of tests passes, run it on some of your favorite examples.

(define DICT-FILENAME "/usr/share/dict/words")
(define DICT (read-lines DICT-FILENAME))

; words->strings: List-of-words -> List-of-strings
; Returns of list of strings, the String representation of each Word in `words`
(check-expect (words->strings '()) '())
(check-expect (words->strings (list (list "a" "c" "t")
                                    (list "c" "a" "t")
                                    (list "b" "u" "n")))
              (list "act" "cat" "bun"))
(define (words->strings words)
  (cond [(empty? words) '()]
        [(cons? words)
         (cons (word->string (first words))
               (words->strings (rest words)))]))

; word->string: Word -> String
; converts `w` into a String
(check-expect (word->string '()) "")
(check-expect (word->string (list "a" "c" "t")) "act")
(define (word->string w)
  (implode w))

; string->word: String -> Word
; convert `s` to the chosen Word representation
(check-expect (string->word "") '())
(check-expect (string->word "act") (list "a" "c" "t"))
(define (string->word s)
  (explode s))

; alternative-words: String -> [String]
; Returns a list of words, in the dictionary, that are permutaions of `s`
(define (alternative-words s)
  (in-dictionary
   (words->strings (arrangements (string->word s)))))

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

; arrangements: Word -> [Word]
; Given a Word `w`, computes all permutations of it
(define (arrangements w)
  (cond [(empty? w) (list '())]
        [else (insert-everywhere/in-all-words (first w)
                                              (arrangements (rest w)))]))

; insert-everywhere/in-all-words: 1String [Word] -> [Word]
; Produces a new list of words, where `l` is inserted at the beginning, between each
; letter and at the end of each word in `words`
(check-expect (insert-everywhere/in-all-words "a" '()) '())
(check-expect (insert-everywhere/in-all-words "a" (list (list "b")))
              (list (list "a" "b") (list "b" "a")))
(check-expect (insert-everywhere/in-all-words "a" (list (list "b" "c")))
              (list (list "a" "b" "c")
                    (list "b" "a" "c")
                    (list "b" "c" "a")))
(check-expect (insert-everywhere/in-all-words "a" (list (list "b" "c" "d")))
              (list (list "a" "b" "c" "d")
                    (list "b" "a" "c" "d")
                    (list "b" "c" "a" "d")
                    (list "b" "c" "d" "a")))
(define (insert-everywhere/in-all-words l words)
  (cond [(empty? words) '()]
        [(cons? words)
         (append (insert-everywhere/word l (first words))
                 (insert-everywhere/in-all-words l (rest words)))]))

; insert-everywhere/word: 1String Word -> [Word]
; Takes `l` and inserts it into each possible place of `w`, producing a new list of
; words
(check-expect (insert-everywhere/word "a" '()) (list (list "a")))
(check-expect (insert-everywhere/word "a" (list "b"))
              (list (list "a" "b")
                    (list "b" "a")))
(check-expect (insert-everywhere/word "a" (list "b" "c"))
              (list (list "a" "b" "c")
                    (list "b" "a" "c")
                    (list "b" "c" "a")))
(check-expect (insert-everywhere/word "a" (list "b" "c" "d"))
              (list (list "a" "b" "c" "d")
                    (list "b" "a" "c" "d")
                    (list "b" "c" "a" "d")
                    (list "b" "c" "d" "a")))
(define (insert-everywhere/word l w)
  (cond [(empty? w) (list (list l))]
        [(cons? w)
         (cons (cons l w)
               (insert-at-front/words (first w)
                                      (insert-everywhere/word l (rest w))))]))

; insert-at-front/words: 1String [Word] -> [Word]
; Puts `l` in front (at the start) of each word in `words`
(check-expect (insert-at-front/words "a" '()) '())
(check-expect (insert-at-front/words "a" (list (list "b"))) (list (list "a" "b")))
(check-expect (insert-at-front/words "a" (list (list "b" "a" "t") (list "c" "a" "t")))
              (list (list "a" "b" "a" "t")
                    (list "a" "c" "a" "t")))
(define (insert-at-front/words l words)
  (cond [(empty? words) '()]
        [(cons? words)
         (cons (cons l (first words))
               (insert-at-front/words l (rest words)))]))
