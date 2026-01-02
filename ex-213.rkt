#lang htdp/bsl+

;;; Design `insert-everywhere/in-all-words`. It consumes a 1String and a list of words.
;;; The result is a list of words like its second argument, but with the first argument
;;; inserted at the beginning, between all letters, and at the end of all words of the
;;; given list.
;;;
;;; Start with a complete wish-list entry. Supplement it with tests for empty lists, a
;;; list with a one-letter word, and another list with a two-letter word, and the like.
;;; Before you continue, study the following three hints carefully:
;;;
;;; HINTS:
;;; 1. Reconsider the example from above. It says that "d" needs to be inserted into the
;;;    words `(list "e" "r")`. The following application is therefore one natural candidate
;;;    for an example:
;;;
;;;    (insert-everywhere/in-all-words "d"
;;;                                    (cons (list "e" "r")
;;;                                          (cons (list "r" "e")
;;;                                                '())))
;;;
;;; 2. You want to use the BSL+ operation `append`, which consumes two lists and produces
;;;    the concatenation of two lists:
;;;
;;;    > (append (list "a" "b" "c") (list "d" "e"))
;;;      (list "a" "b" "c" "d" "e")
;;;
;;; 3. The solution of this exercise is a series of functions. Patiently stick to the
;;;    design recipe and systematically work through your wish list.

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
