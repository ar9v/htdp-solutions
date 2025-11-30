#lang htdp/bsl

;;; Create an element of List-of-names that contains five Strings. Sketch a box
;;; representation of the list similar to those found in figure 44.

(define lon
  (cons "Matt"
        (cons "Shriram"
              (cons "Bobby"
                    (cons "Matthias"
                          (cons "Daniel" '()))))))

;;; Gonna skip the sketch again, chief.


;;; Explain why (cons "1" (cons "2" '())) is an element of List-of-names and why
;;; (cons 2 '()) isn't

;;; A:
;;; The first is a List-of-names because
;;;   1. '() is a List-of-names (applying the first clause in the itemization)
;;;   2. (cons "2" '()) is a List-of-names by applying the second clause and then the first
;;;   3. So (cons "1" (cons "2" '())) is a List-of-names by applying the second item
;;;      as well, since we established that (2) is a List-of-names.
;;;
;;; The second isn't because 2 is not a String.
