#lang htdp/bsl+

(require 2htdp/itunes)

;;; Design the function `total-time`, which consumes an element of LTracks and produces
;;; the total amount of play time. Once the program is done, compute the total play time
;;; of your iTunes collection.

(define ITUNES-LOCATION "itunes.xml")
(define ITUNES-TRACKS
  (read-itunes-as-tracks ITUNES-LOCATION))

;;; Dates
(define date-1 (create-date 2015 3 10 11 45 3))
(define date-2 (create-date 2002 7 17 3 55 14))
(define date-3 (create-date 2011 5 17 17 35 13))
(define last-christmas (create-date 2024 12 25 20 20 20))
(define christmas-eve (create-date 2025 12 24 22 0 0))
(define new-year (create-date 2026 1 1 0 0 0))

;;; Tracks
(define giant-steps
  (create-track "Giant Steps" "John Coltrane" "Giant Steps" 286000 1 date-1 100 new-year))

(define last-christmas-track
  (create-track "Last Christmas"
              "Wham"
              "Music from the Edge of Heaven"
              267000
              8
              last-christmas
              25
              christmas-eve))

(define wild-child
  (create-track "Wild Child" "Enya" "A Day Without Rain" 227996 2 date-2 20 date-3))

;;; LTracks
(define no-tracks empty)
(define test-tracks
  (list giant-steps
        last-christmas-track
        wild-child))

; total-time: LTracks -> Number
; Computes the total play time
(check-expect (total-time no-tracks) 0)
(check-expect (total-time test-tracks)
              (+ 227996 267000 286000))
(define (total-time tracks)
  (cond [(empty? tracks) 0]
        [(cons? tracks)
         (+ (track-time (first tracks))
            (total-time (rest tracks)))]))

;; (total-time ITUNES-TRACKS) -> 467897977 in the book-provided itunes.xml
