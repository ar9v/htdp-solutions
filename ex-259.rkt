#lang htdp/isl

(require 2htdp/batch-io)

;;; Use a `local` expression to organize the functions for rearranging words from chapter
;;; 12.4

(define DICT-FILENAME "/usr/share/dict/words")
(define DICT (read-lines DICT-FILENAME))

; alternative-words: String -> [String]
; Returns a list of words, in the dictionary, that are permutaions of `word`
(define (alternative-words word)
  (local ((define (in-dictionary? s) (member? s DICT))
          (define word-letters (explode word))
          (define letter-permutations (arrangements word-letters))
          (define words (map implode letter-permutations))
          (define real-words (filter in-dictionary? words)))
    real-words))

; arrangements: [List-of T] -> [[List-of T]]
; Given a list, computes all permutations of it
(define (arrangements l)
  (local (; insert-everywhere/all: Any [List-of [List-of Any]] -> [List-of [List-of Any]]
          ; Produces a new list of lists, where `i` is inserted at the beginning,
          ; between each item and at the end of each item in `l`
          (define (insert-everywhere/all i lol)
            (local ((define (combine l res) (append (insert-everywhere/list i l) res)))
              (foldr combine '() lol)))

          ; insert-everywhere/list: Any [List-of Any] -> [List-of [List-of Any]]
          ; Takes `i` and inserts it into each possible place of `l`, producing a new list
          ; of words
          (define (insert-everywhere/list i l)
            (local ((define (combine x res)
                      (cons (cons i (cons x (rest (first res))))
                            (insert-at-front/lists x res))))
              (foldr combine (list (list i)) l)))

          ; insert-at-front/lists: Any [List-of [List-of Any]] -> [List-of [List-of Any]]
          ; Puts `i` in front (at the start) of each list in `lists`
          (define (insert-at-front/lists i ls)
            (local ((define (cons-i l) (cons i l))) (map cons-i ls))))

    (foldr insert-everywhere/all (list '()) l)))
