#lang htdp/bsl+

(require 2htdp/itunes)

;;; Design `select-album-date`. The function consumes the title of an album, a date, and
;;; an LTracks. It extracts from the latter the list of tracks that belong to the given
;;; album and have been played after the given date.
;;;
;;; HINT: You must design a function that consumes two Dates and determines whether the
;;; first occurs before the second.

(define SECONDS-IN-MINUTE 60)
(define MINUTES-IN-HOUR 60)

(define ITUNES-LOCATION "itunes.xml")
(define ITUNES-TRACKS
  (read-itunes-as-tracks ITUNES-LOCATION))

;;; Dates
(define date-1 (create-date 2002 3 10 11 45 3))
(define date-2 (create-date 2007 7 17 16 55 14))
(define date-3 (create-date 2011 5 2 12 35 13))
(define date-4 (create-date 2015 9 1 13 35 20))

;;; Tracks
(define giant-steps
  (create-track "Giant Steps" "John Coltrane" "Giant Steps" 286000 1 date-1 100 date-2))

(define cousin-mary
  (create-track "Cousin Mary" "John Coltrane" "Giant Steps" 346000 2 date-1 80 date-3))

(define naima
  (create-track "Naima" "John Coltrane" "Giant Steps" 261000 6 date-1 70 date-4))


;;; LTracks
(define no-tracks empty)
(define test-tracks
  (list giant-steps
        cousin-mary
        naima))

; select-album: String LTracks -> LTracks
; Given an `album` and a list of `tracks`, returns the tracks that belong to `album`
(define (select-album album tracks)
  (cond [(empty? tracks) '()]
        [(cons? tracks)
         (if (string=? album (track-album (first tracks)))
             (cons (first tracks) (select-album album (rest tracks)))
             (select-album album (rest tracks)))]))

; select-album-date: String Date LTracks -> LTracks
; Picks out all tracks in `tracks` that belong to `album` and have been played after
; `date`
(check-expect (select-album-date "Giant Steps" date-1 no-tracks) '())
(check-expect (select-album-date "Foxtrot" date-1 test-tracks) '())
(check-expect (select-album-date "Giant Steps" date-1 test-tracks)
              test-tracks)
(check-expect (select-album-date "Giant Steps" date-2 test-tracks)
              (list cousin-mary naima))
(check-expect (select-album-date "Giant Steps" date-3 test-tracks)
              (list naima))
(check-expect (select-album-date "Giant Steps" date-4 test-tracks)
              no-tracks)
(define (select-album-date album date tracks)
  (select-after-date date (select-album album tracks)))

; select-after-date: Date LTracks -> LTracks
; Picks out tracks from `tracks` which have been played after `date`
(check-expect (select-after-date date-1 no-tracks) no-tracks)
(check-expect (select-after-date date-1 test-tracks) test-tracks)
(check-expect (select-after-date date-2 test-tracks) (cdr test-tracks))
(define (select-after-date date tracks)
  (cond [(empty? tracks) '()]
        [(cons? tracks)
         (if (date< date (track-played (first tracks)))
             (cons (first tracks) (select-after-date date (rest tracks)))
             (select-after-date date (rest tracks)))]))

; date<: Date Date -> Boolean
; True if `d1` happened before `d2`
(check-expect (date< date-1 date-2) #true)
(check-expect (date< date-3 date-2) #false)
(check-expect (date< date-1 date-1) #false)
(check-expect
 (date< (create-date 2005 1 1 1 1 1)
        (create-date 2005 1 1 1 1 2))
 #true)
(define (date< d1 d2)
  (or (< (date-year d1) (date-year d2))
      (and (= (date-year d1) (date-year d2))
           (< (date-month d1) (date-month d2)))
      (and (= (date-year d1) (date-year d2))
           (= (date-month d1) (date-month d2))
           (< (date-day d1) (date-day d2)))
      (and (= (date-year d1) (date-year d2))
           (= (date-month d1) (date-month d2))
           (= (date-day d1) (date-day d2))
           (< (timestamp d1) (timestamp d2)))))

; timestamp: Date -> Number
; Given a `date`, returns the elapsed seconds since midnight (courtesy of ex-081.rkt)
(check-expect (timestamp (create-date 2005 1 1 12 30 2)) 45002)
(check-expect (timestamp (create-date 2005 1 1 0 0 45)) 45)
(check-expect (timestamp (create-date 2005 1 1 0 2 45)) (+ (* 2 60) 45))
(check-expect (timestamp (create-date 2005 1 1 4 6 45))
              (+ (* 4 (sqr 60)) (* 6 60) 45))
(define (timestamp d)
  (+ (* (date-hour d) MINUTES-IN-HOUR SECONDS-IN-MINUTE)
     (* (date-minute d) SECONDS-IN-MINUTE)
     (date-second d)))

;; (select-album-date "Foxtrot" date-3 ITUNES-TRACKS)
