#lang htdp/bsl+

(require 2htdp/itunes)

;;; Design `total-time/list`, which consumes an LLists and produces the total amount of
;;; play time.

(define ITUNES-LOCATION "itunes.xml")
(define LIST-TRACKS
  (read-itunes-as-lists ITUNES-LOCATION))

(define date-1 (create-date 2002 3 10 11 45 3))
(define date-2 (create-date 2007 7 17 16 55 14))
(define date-3 (create-date 2011 5 2 12 35 13))
(define date-4 (create-date 2015 9 1 13 35 20))
(define last-christmas (create-date 2024 12 25 20 20 20))
(define christmas-eve (create-date 2025 12 24 22 0 0))

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

(define no-llists '())
(define test-llists
  (list giant-steps cousin-mary naima last-christmas-track wild-child))


; total-time/list: LLists -> Number
; Given an llists, produces the total amount of play time
(check-expect (total-time/list no-llists) 0)
(check-expect (total-time/list test-llists)
              (+ 286000 346000 261000 267000 227996))
(define (total-time/list llists)
  (cond [(empty? llists) 0]
        [(cons? llists)
         (+ (if (cons? (assoc "Total Time" (first llists)))
                (second (assoc "Total Time" (first llists)))
                0)
            (total-time/list (rest llists)))]))

;;; Once you have completed the design, compute the total play time of your iTunes
;;; collection. Compare this result with the time that the `total-time` function from
;;; exercise 200 computes. Why is there a difference?
;;;
;;; A:
;;; There's a difference because reading the iTunes collection with `read-itunes-as-tracks`
;;; yields a shorter list. That, in turn, is due to it using the `track` struct's checked
;;; constructor, which will discard any entries that do not fulfill all of the field
;;; predicates.
;;;
;;; On the other hand, reading the XML file as a LLists simply builds out the list of
;;; Association lists, which have no predicates attached to them.

;; (total-time/list LIST-TRACKS) ; -> 494030470
