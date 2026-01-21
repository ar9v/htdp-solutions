#lang htdp/isl+

(require 2htdp/abstraction)

;;; Define `find-name`. The function consumes a name and a list of names. It retrieves the
;;; first name on the latter that is equal to, or an extension of, the former.

; find-name: String [List-of String] -> Boolean
; Finds the first name in `names` that is equal to or an extension of `name`
(check-expect (find-name "Ric" '()) #false)
(check-expect (find-name "Ric" (list "Mike" "Matt")) #false)
(check-expect (find-name "Ric" (list "Mike" "Ric" "Matt")) "Ric")
(check-expect (find-name "Ric" (list "Mike" "Ricardo" "Matt")) "Ricardo")
(define (find-name name l)
  (for/or [(n l)] (if (string-contains? name n) n #false)))

;;; Define a function that ensures that no name on some list of names exceeds some given
;;; width. Compare with exercise 271.

(check-expect (all-shorter-than? 5 '("Ric" "Bob" "Mike")) #true)
(check-expect (all-shorter-than? 5 '("Ric" "Bob" "Mike" "Matthew")) #false)
(define (all-shorter-than? len names)
  (for/and [(name names)] (< (string-length name) len)))
