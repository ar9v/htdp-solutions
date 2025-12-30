#lang htdp/bsl+

(require 2htdp/itunes)

;;; Develop examples of LAssoc and LLists, that is, the list representation of tracks and
;;; lists of such tracks.

;;; Dates
(define date-1 (create-date 2002 3 10 11 45 3))
(define date-2 (create-date 2007 7 17 16 55 14))
(define date-3 (create-date 2011 5 2 12 35 13))
(define date-4 (create-date 2015 9 1 13 35 20))
(define last-christmas (create-date 2024 12 25 20 20 20))
(define christmas-eve (create-date 2025 12 24 22 0 0))

; An LAssoc is one of:
; -- '()
; -- (cons Association LAssoc)
(define empty-lassoc '())
(define giant-steps
  (list (list "Name" "Giant Steps")
        (list "Artist" "John Coltrane")
        (list "Composer" "John Coltrane")
        (list "Album" "Giant Steps")
        (list "Total Time" 286000)
        (list "Track Count" 1)
        (list "Date Added" date-1)
        (list "Play Count" 100)
        (list "Play Date UTC" date-2)))
(define cousin-mary
  (list (list "Name" "Cousin Mary")
        (list "Artist" "John Coltrane")
        (list "Composer" "John Coltrane")
        (list "Album" "Giant Steps")
        (list "Total Time" 346000)
        (list "Track Count" 2)
        (list "Date Added" date-1)
        (list "Play Count" 80)
        (list "Play Date UTC" date-3)))
(define naima
  (list (list "Name" "Naima")
        (list "Artist" "John Coltrane")
        (list "Composer" "John Coltrane")
        (list "Album" "Giant Steps")
        (list "Total Time" 261000)
        (list "Track Count" 6)
        (list "Date Added" date-1)
        (list "Play Count" 70)
        (list "Play Date UTC" date-4)))

(define last-christmas-track
  (list (list "Name" "Last Christmas")
        (list "Artist" "Wham")
        (list "Composer" "George Michael")
        (list "Album" "Music from the Edge of Heaven")
        (list "Total Time" 267000)
        (list "Track Count" 8)
        (list "Date Added" last-christmas)
        (list "Play Count" 25)
        (list "Play Date UTC" christmas-eve)))

(define wild-child
  (list (list "Name" "Wild Child")
        (list "Artist" "Enya")
        (list "Composer" "Enya")
        (list "Album" "A Day Without Rain")
        (list "Total Time" 227996)
        (list "Track Count" 2)
        (list "Date Added" date-2)
        (list "Play Count" 20)
        (list "Play Date UTC" date-3)))

; An Association is a list of two items:
; -- (cons String (cons BSDN '()))

; A BSDN is one of:
; -- Boolean
; -- Number
; -- String
; -- Date

; An LLists is one of
; -- '()
; -- (cons LAssoc LLists)
(define no-llists '())
(define test-llists
  (list giant-steps cousin-mary naima last-christmas-track wild-child))
