#lang htdp/bsl+

;;; While the important data definitions are already provided, the first step of the
;;; design recipe is still incomplete. Make up examples of Dates, Tracks, and LTracks.
;;;
;;; These examples come in handy for the following exercises as inputs.

;;; Dates
(define-struct date [year month day hour minute second])
; A Date is a structure:
;   (make-date N N N N N N)
;
; interpretation: An instance records six pieces of information:
; The date's year, month (between 1 and 12 inclusive), day (between 1 and 31), hour
; (between 0 and 23), minute (between 0 and 59), and second (also between 0 and 59).
(define date-1 (make-date 2015 3 10 11 45 3))
(define date-2 (make-date 2002 7 17 3 55 14))
(define date-3 (make-date 2011 5 17 17 35 13))
(define last-christmas (make-date 2024 12 25 20 20 20))
(define christmas-eve (make-date 2025 12 24 22 0 0))
(define new-year (make-date 2026 1 1 0 0 0))


;;; Tracks
(define-struct track
  [name artist album time track# added play# played])
; A Track is a structure:
;   (make-track String String String N N Date N Date)
;
; interpretation: An instance records in order: the track's title, its producing artist,
; to which album it belongs, its playing time in milliseconds, its position within the
; album, the date it was added, how often it has been played, and the date when it was
; last played
(define giant-steps
  (make-track "Giant Steps" "John Coltrane" "Giant Steps" 286000 1 date-1 100 new-year))

(define last-christmas-track
  (make-track "Last Christmas"
              "Wham"
              "Music from the Edge of Heaven"
              267000
              8
              last-christmas
              25
              christmas-eve))

(define wild-child
  (make-track "Wild Child" "Enya" "A Day Without Rain" 227996 2 date-2 20 date-3))

;;; LTracks
(define no-tracks empty)
(define test-tracks
  (list giant-steps
        last-christmas-track
        wild-child))
