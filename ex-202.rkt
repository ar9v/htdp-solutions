#lang htdp/bsl+

(require 2htdp/itunes)

;;; Design `select-album`. The function consumes the title of an album and an LTracks. It
;;; extracts from the latter the list of tracks that belong to the given album.

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

(define naima
  (create-track "Naima" "John Coltrane" "Giant Steps" 261000 6 date-1 70 new-year))

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
        naima
        last-christmas-track
        wild-child))

; select-album: String LTracks -> LTracks
; Given an `album` and a list of `tracks`, returns the tracks that belong to `album`
(check-expect (select-album "Giant Steps" no-tracks) '())
(check-expect (select-album "Foxtrot" test-tracks) '())
(check-expect (select-album "Giant Steps" test-tracks) (list giant-steps naima))
(check-expect (select-album "Music from the Edge of Heaven" test-tracks)
              (list last-christmas-track))
(define (select-album album tracks)
  (cond [(empty? tracks) '()]
        [(cons? tracks)
         (if (string=? album (track-album (first tracks)))
             (cons (first tracks) (select-album album (rest tracks)))
             (select-album album (rest tracks)))]))

;; (select-album "Foxtrot" ITUNES-TRACKS)
