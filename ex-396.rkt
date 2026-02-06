#lang htdp/isl+

(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

;;; The goal of this exercise is to design `compare-word`, the central function. It
;;; consumes the word to be guessed, a word `s` that represents how much/little the
;;; guessing player has discovered, and the current guess. The function produces `s` with
;;; all "_" where the guess revealed a letter.

(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed

; play: HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))

          ; do-nothing: HM-Word -> HM-Word
          (define (do-nothing s) s)

          ; checked-compare: HM-Word KeyEvent -> HM-Word
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
               [to-draw render-word]
               [on-tick do-nothing 1 time-limit]
               [on-key checked-compare]))))

; render-word: HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; compare-word: HM-Word HM-Word Letter -> HM-Word
(check-expect (compare-word '() '() "a") '())
(check-expect (compare-word (explode "cow") '("_" "o" "_") "C") '("_" "o" "_"))
(check-expect (compare-word (explode "babble") '("_" "a" "_" "_" "_" "e") "b")
              '("b" "a" "b" "b" "_" "e"))
(check-expect (compare-word (explode "blue") '("b" "l" "u" "_") "r")
              '("b" "l" "u" "_"))
(check-expect (compare-word (explode "cow") '("c" "o" "_") "c") '("c" "o" "_"))
(define (compare-word word current guess)
  (cond [(empty? word) '()]
        [else (cons (if (string=? (first word) guess) guess (first current))
                    (compare-word (rest word) (rest current) guess))]))

;;; Once you have designed the function, run the program like this:
(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))
(play (list-ref AS-LIST (random SIZE)) 10)
