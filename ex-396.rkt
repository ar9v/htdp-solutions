#lang htdp/isl+

(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

;;; The goal of this exercise is to design `compare-word`, the central function. It
;;; consumes the word to be guessed, a word `s` that represents how much/little the
;;; guessing player has discovered, and the current guess. The function produces `s` with
;;; all "_" where the guess revealed a letter.

(define FONT-SIZE 22)
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define SCENE-SIZE (* (/ (length LETTERS) 2) FONT-SIZE))

(define BACKGROUND (empty-scene SCENE-SIZE SCENE-SIZE))

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed

(define-struct hm-state [word time])
; A HM-State is a structure
;  (make-hm-state HM-Word N)

; play: HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))

          ; tick: HM-State -> HM-State
          (define (tick s) (make-hm-state (hm-state-word s) (sub1 (hm-state-time s))))

          ; checked-compare: HM-State KeyEvent -> HM-State
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (make-hm-state (compare-word the-word (hm-state-word current-status) ke)
                               (hm-state-time current-status))
                current-status))

          ; game-over?: HM-State -> Boolean
          (define (game-over? s) (or (game-won? s) (game-lost? s)))
          (define (game-won? s) (equal? (hm-state-word s) the-word))
          (define (game-lost? s) (= (hm-state-time s) 0))

          ; render-final: HM-Word -> Image
          (define (render-final w)
            (overlay/align
             'center 'top
             (if (game-won? w)
                 (text "You won!" FONT-SIZE "green")
                 (text "Time's up!" FONT-SIZE "red"))
             (render-state w))))

    (implode
     (hm-state-word
      (big-bang (make-hm-state the-guess time-limit) ; HM-State
                [to-draw render-state]
                [on-tick tick 1 time-limit]
                [on-key checked-compare]
                [stop-when game-over? render-final])))))

; render-word: HM-State -> Image
(define (render-state s)
  (overlay/align
   'center 'center
   (text (number->string (hm-state-time s)) FONT-SIZE "black")
   (overlay/align
    'center 'bottom (text (implode (inject " " (hm-state-word s))) FONT-SIZE "black")
    BACKGROUND)))

; compare-word: HM-Word HM-Word Letter -> HM-Word
(check-expect (compare-word '() '() "a") '())
(check-expect (compare-word (explode "cow") '("_" "o" "_") "!") '("_" "o" "_"))
(check-expect (compare-word (explode "babble") '("_" "a" "_" "_" "_" "e") "b")
              '("b" "a" "b" "b" "_" "e"))
(check-expect (compare-word (explode "blue") '("b" "l" "u" "_") "r")
              '("b" "l" "u" "_"))
(check-expect (compare-word (explode "cow") '("c" "o" "_") "c") '("c" "o" "_"))
(define (compare-word word current guess)
  (cond [(empty? word) '()]
        [else (cons (if (string=? (first word) guess) guess (first current))
                    (compare-word (rest word) (rest current) guess))]))

(define (inject x l)
  (cond [(empty? l) '()]
        [else
         (cons (first l)
               (cons x (inject x (rest l))))]))

;;; Once you have designed the function, run the program like this:
(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))
;; (play (list-ref AS-LIST (random SIZE)) 10)
