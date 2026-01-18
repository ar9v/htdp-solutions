#lang htdp/isl+

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
  (ormap (λ (n) (string-contains? name n)) names))

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
  (andmap (λ (s0) (string=? s (string-ith s0 0))) strs))
