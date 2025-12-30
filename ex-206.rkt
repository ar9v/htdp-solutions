#lang htdp/bsl+

(require 2htdp/itunes)

;;; Design the function `find-association`. It consumes three arguments: a String called
;;; `key`, an LAssoc, and an element of Any called `default`. It produces the first
;;; Association whose first item is equal to `key`, or `default` if there is no such
;;; Association.

; Dates
(define date-1 (create-date 2002 3 10 11 45 3))
(define date-2 (create-date 2007 7 17 16 55 14))

; Example LAssocs
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

; find-association: String LAssoc Any -> Association | Any
; Returns the first Association in `lassoc` that matches `key`, or `default` if there is
; no match.
(check-expect (find-association "any" empty-lassoc "default") "default")
(check-expect (find-association "Not Found" giant-steps "default") "default")
(check-expect (find-association "Composer" giant-steps "default")
              (list "Composer" "John Coltrane"))
(define (find-association key lassoc default)
  (cond [(empty? lassoc) default]
        [(cons? lassoc)
         (if (string=? key (first (first lassoc)))
             (first lassoc)
             (find-association key (rest lassoc) default))]))
