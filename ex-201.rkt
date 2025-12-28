#lang htdp/bsl+

(require 2htdp/itunes)

;;; Design `select-all-album-titles`. The function consumes an LTracks and produces a
;;; list of album titles as a list of strings.

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

; select-all-album-titles: LTracks -> List-of-strings
; Returns all albums referenced in `tracks`
(check-expect (select-all-album-titles no-tracks) '())
(check-expect (select-all-album-titles test-tracks)
              (list "Giant Steps"
                    "Giant Steps"
                    "Music from the Edge of Heaven"
                    "A Day Without Rain"))
(define (select-all-album-titles tracks)
  (cond [(empty? tracks) '()]
        [(cons? tracks)
         (cons (track-album (first tracks))
               (select-all-album-titles (rest tracks)))]))

;;; Also design the function `create-set`. It consumes a List-of-strings and constructs one
;;; that contains every String from the given list exactly once.
;;;
;;; HINT: If String `s` is at the front of the given list and occurs in the rest of the
;;; list, too, `create-set` does not keep `s`.

; create-set: List-of-strings -> List-of-strings
; Given a List-of-strings, `los`, produces a new list, where each element is only there
; exactly once.
;
; assumption: This removes the first occurrence(s) of duplicates
(check-expect (create-set '()) '())
(check-expect (create-set (list "a")) (list "a"))
(check-expect (create-set (list "a" "b")) (list "a" "b"))
(check-expect (create-set (list "a" "b" "b" "c")) (list "a" "b" "c"))
(check-expect (create-set (list "a" "a" "b" "c")) (list "a" "b" "c"))
(check-expect (create-set (list "a" "b" "c" "c")) (list "a" "b" "c"))
(check-expect (create-set (list "a" "b" "a")) (list "b" "a"))
(define (create-set los)
  (cond [(empty? los) '()]
        [(cons? los)
         (if (member? (first los) (rest los))
             (create-set (rest los))
             (cons (first los) (create-set (rest los))))]))

;;; Finally, design `select-album-titles/unique`, which consumes an LTracks and produces
;;; a list of unique album titles. Use this function to determine all album titles in
;;; your iTunes collection and also find out how many distinct albums it contains.

; select-album-titles/unique: LTracks -> List-of-strings
; Like select-all-album-titles, but the results are unique
(check-expect (select-album-titles/unique no-tracks) '())
(check-expect (select-album-titles/unique test-tracks)
              (list "Giant Steps"
                    "Music from the Edge of Heaven"
                    "A Day Without Rain"))
(define (select-album-titles/unique tracks)
  (create-set (select-all-album-titles tracks)))


;; (select-album-titles/unique ITUNES-TRACKS)
