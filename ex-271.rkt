#lang htdp/isl

;;; User `ormap` to define `find-name`. The function consumes a name and a list of names.
;;; It determines whether any of the names on the latter are equal to or an extension of
;;; the former.

; find-name: String [List-of String] -> Boolean
; Determines whether any name in `names` is equal to or an extension of `name`
(check-expect (find-name "Ric" '()) #false)
(check-expect (find-name "Ric" (list "Mike" "Matt")) #false)
(check-expect (find-name "Ric" (list "Mike" "Ric" "Matt")) #true)
(check-expect (find-name "Ric" (list "Mike" "Ricardo" "Matt")) #true)
(define (find-name name names)
  (local ((define (eq-or-extension? n) (string-contains? name n)))
    (ormap eq-or-extension? names)))

;;; With `andmap` you can define a function that checks all names on a list of names start
;;; with the letter "a"

; all-start-with?: 1String [List-of String] -> Boolean
; Determines if all strings in `strs` start with 1String `s`
;
; constraint: `strs` is made up of non-empty strings
(check-expect (all-start-with? "a" '()) #true)
(check-expect (all-start-with? "a" '("albatross" "amicable")) #true)
(check-expect (all-start-with? "a" '("albatross" "begin" "amicable")) #false)
(define (all-start-with? s strs)
  (local ((define (start-with-s? s0) (string=? s (string-ith s0 0))))
    (andmap start-with-s? strs)))

;;; Should you use `ormap` or `andmap` to define a function that ensures that no name on
;;; some list exceeds a given width?
;;;
;;; A:
;;; Any will do, because they are duals! Suppose you want the width to be 5
;;;
;;; with andmap:
;;; (andmap shorter-than-5? names)
;;;
;;; with ormap:
;;; (not (ormap longer-or-5? names))
;;;
;;; In this case, `andmap` is preferable since it is easier to read.
