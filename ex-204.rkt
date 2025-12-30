#lang htdp/bsl+

(require 2htdp/itunes)

;;; Design `select-albums`. The function consumes an element of LTracks. It produces a
;;; list of LTracks, one per album. Each album is uniquely identified by its title and
;;; shows up in the result only once.
;;;
;;; HINTS:
;;;  1. You want to use some of the solutions of the preceding exercises.
;;;  2. The function that groups consumes two lists: the list of album titles and the
;;;     list of tracks; it considers the latter as atomic until it is handed over to an
;;;     auxiliary function. See exercise 196.

(define ITUNES-LOCATION "itunes.xml")
(define ITUNES-TRACKS
  (read-itunes-as-tracks ITUNES-LOCATION))

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

; select-albums: LTracks -> [LTracks]
; Given a list of all tracks, `tracks`, groups them into lists of tracks corresponding to
; their albums.
(check-expect (select-albums no-tracks) no-tracks)
(check-satisfied (select-albums test-tracks)
                 contains-grouped-test-tracks?)
(define (select-albums tracks)
  (group-by-albums (select-album-titles/unique tracks) tracks))

; select-album-titles/unique: LTracks -> List-of-strings
; Like select-all-album-titles, but the results are unique
(define (select-album-titles/unique tracks)
  (create-set (select-all-album-titles tracks)))

; create-set: List-of-strings -> List-of-strings
; Given a List-of-strings, `los`, produces a new list, where each element is only there
; exactly once.
;
; assumption: This removes the first occurrence(s) of duplicates
(define (create-set los)
  (cond [(empty? los) '()]
        [(cons? los)
         (if (member? (first los) (rest los))
             (create-set (rest los))
             (cons (first los) (create-set (rest los))))]))

; select-all-album-titles: LTracks -> List-of-strings
; Returns all albums referenced in `tracks`
(define (select-all-album-titles tracks)
  (cond [(empty? tracks) '()]
        [(cons? tracks)
         (cons (track-album (first tracks))
               (select-all-album-titles (rest tracks)))]))

; group-by-albums: [String] LTracks -> [LTracks]
; Groups `tracks` into a list of tracks corresponding to `albums`
(check-expect (group-by-albums '() no-tracks) no-tracks)
(check-expect (group-by-albums '() test-tracks) no-tracks)
(check-expect (group-by-albums (list "Foxtrot") test-tracks) (list no-tracks))
(check-expect (group-by-albums (list "Giant Steps"
                                     "Music from the Edge of Heaven"
                                     "A Day Without Rain")
                               test-tracks)
              (list (list giant-steps cousin-mary naima)
                    (list last-christmas-track)
                    (list wild-child)))
(define (group-by-albums albums tracks)
  (cond [(empty? albums) '()]
        [(cons? albums)
         (cons (select-album (first albums) tracks)
               (group-by-albums (rest albums) tracks))]))

; select-album: String LTracks -> LTracks
; Given an `album` and a list of `tracks`, returns the tracks that belong to `album`
(define (select-album album tracks)
  (cond [(empty? tracks) '()]
        [(cons? tracks)
         (if (string=? album (track-album (first tracks)))
             (cons (first tracks) (select-album album (rest tracks)))
             (select-album album (rest tracks)))]))

; contains-grouped-test-tracks?: [LTracks] -> Boolean
; Predicate that checks whether a given list of LTracks actually groups `test-tracks`
; correctly. We need this because `create-set` will mess up the ordering of the list of
; albums.
;
; (Still only slightly better, because the inner lists themselves are also sensitive to
; ordering)
(define (contains-grouped-test-tracks? lotracks)
  (and (= (length lotracks) 3)
       (member? (list giant-steps cousin-mary naima) lotracks)
       (member? (list last-christmas-track) lotracks)
       (member? (list wild-child) lotracks)))

;; (select-albums ITUNES-TRACKS)
