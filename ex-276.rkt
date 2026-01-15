#lang htdp/isl

(require 2htdp/itunes)

;;; Chapter 12.2 explains how to analyze the information in an iTunes XML library.
;;;
;;; - Design `select-album-date`. The function consumes the title of an album, a date,
;;;   and an LTracks. It extracts from the latter the list of tracks from the given album
;;;   that have been played after the date
;;;
;;; - Design `select-albums`. The function consumes an LTracks. It produces a list of
;;;   LTracks, one per album. Each album is uniquely identified by its title and shows
;;;   up in the result only once.

(define ITUNES-LOCATION "itunes.xml")
(define ITUNES-TRACKS
  (read-itunes-as-tracks ITUNES-LOCATION))

(define SECONDS-IN-MINUTE 60)
(define MINUTES-IN-HOUR 60)

;;; Dates
(define date-1 (create-date 2002 3 10 11 45 3))
(define date-2 (create-date 2007 7 17 16 55 14))
(define date-3 (create-date 2011 5 2 12 35 13))
(define date-4 (create-date 2015 9 1 13 35 20))
(define last-christmas (create-date 2024 12 25 20 20 20))
(define christmas-eve (create-date 2025 12 24 22 0 0))

;;; Tracks
(define giant-steps
  (create-track "Giant Steps" "John Coltrane" "Giant Steps" 286000 1 date-1 100 date-2))

(define cousin-mary
  (create-track "Cousin Mary" "John Coltrane" "Giant Steps" 346000 2 date-1 80 date-3))

(define naima
  (create-track "Naima" "John Coltrane" "Giant Steps" 261000 6 date-1 70 date-4))

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
        cousin-mary
        wild-child
        naima))

; date<: Date Date -> Boolean
; True if `d1` happened before `d2`
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
           (< (date-elapsed-seconds d1) (date-elapsed-seconds d2)))))

; date-elapsed-seconds: Date -> Number
; Given a `date`, returns the elapsed seconds since midnight (courtesy of ex-081.rkt)
(define (date-elapsed-seconds d)
  (+ (* (date-hour d) MINUTES-IN-HOUR SECONDS-IN-MINUTE)
     (* (date-minute d) SECONDS-IN-MINUTE)
     (date-second d)))

; group-by: [X -> Y] [List-of X] -> [List-of Association]
; Given a list of items, uses `f` to determine a key to which an item belongs, and returns
; the list of associations
(define (group-by f l)
  (local [(define (combine i res)
            (local [(define (updater val) (cons i val))]
              (assoc-update res (f i) updater (list i))))]
    (foldr combine '() l)))

; assoc-update: [List-of Association] X [Y -> Z] Z -> [List-of Association]
; Updates `assc` by running `f` on the value for its `key`; uses `fallback` if the key is
; not found
(define (assoc-update assc key updater fallback)
  (local [(define val (assoc key assc))
          (define (combine x res)
            (cons (if (equal? (first x) key) (list key (updater (second x))) x)
                  res))]
    (if (false? val)
        (cons (list key fallback) assc)
        (foldr combine '() assc))))

; select-album-date: String Date LTracks -> LTracks
; Picks out all tracks in `tracks` that belong to `album` and have been played after
; `date`
(check-expect (select-album-date "Giant Steps" date-1 no-tracks) '())
(check-expect (select-album-date "Foxtrot" date-1 test-tracks) '())
(check-expect (select-album-date "Giant Steps" date-1 test-tracks)
              (list giant-steps cousin-mary naima))
(check-expect (select-album-date "Giant Steps" date-2 test-tracks)
              (list cousin-mary naima))
(check-expect (select-album-date "Giant Steps" date-3 test-tracks)
              (list naima))
(check-expect (select-album-date "Giant Steps" date-4 test-tracks)
              no-tracks)
(define (select-album-date album date tracks)
  (local [(define (played-after-date? t) (date< date (track-played t)))
          (define (same-album? t) (string=? (track-album t) album))]
    (filter played-after-date? (filter same-album? tracks))))

; select-albums: LTracks -> [List-of LTracks]
; Given a list of all tracks, `tracks`, groups them into lists of tracks corresponding to
; their albums.
(check-expect (select-albums no-tracks) no-tracks)
(check-satisfied (select-albums test-tracks) contains-grouped-test-tracks?)
(define (select-albums tracks)
  (map second (group-by track-album tracks)))

; contains-grouped-test-tracks?: [LTracks] -> Boolean
; Predicate that checks whether a given list of LTracks actually groups `test-tracks`
; correctly. We need this because `create-set` will mess up the ordering of the list of
; albums.
(define (contains-grouped-test-tracks? lotracks)
  (and (= (length lotracks) 3)
       (member? (list giant-steps cousin-mary naima) lotracks)
       (member? (list last-christmas-track) lotracks)
       (member? (list wild-child) lotracks)))
