#lang htdp/isl+

(require 2htdp/abstraction)

;;; Develop the function `average-age`. It consumes a family tree and the current year.
;;; It produces the average age of all `child` structures in the family tree.

(define-struct no-parent [])
(define NP (make-no-parent))
(define-struct child [father mother name date eyes])
; A FT is one of
; -- NP
; -- (make-child FT FT String N String)

; Data Examples
;; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))

;; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))

;; Youngest Generation:
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; age: N -> [N -> N]
; Returns a function that computes an age with respect to `year`, once its given a
; date of birth
(define (age year) (λ (dob) (- year dob)))

; average-age: Child N -> N
; Produces the average age of all members in the family tree with respect to
; `current-year`
(check-error (average-age NP))
(check-expect (average-age Carl 2026) (- 2026 (child-date Carl)))
(check-expect (average-age Dave 2026) (average (map (age 2026) (ft-dates Dave))))
(check-expect (average-age Gustav 2026)
              (average (map (age 2026) (ft-dates Gustav))))
(define (average-age ft current-year)
  (average (map (age current-year) (ft-dates ft))))

; ft-dates: FT -> [List-of N]
; Given a FT `ft`, returns a list of the dates of birth of all its members
(check-expect (ft-dates Carl) '(1926))
(check-expect (ft-dates Dave) '(1955 1926 1926))
(check-expect (ft-dates Gustav) '(1988 1966 1965 1926 1926))
(define (ft-dates ft)
  (match ft
    [(no-parent) '()]
    [(child f m _n dob _e)
     (append (list dob) (ft-dates f) (ft-dates m))]))

; average: [NonEmptyList-of N] -> N
; Computes the average of the given list of numbers
(define (average ns) (/ (foldr + 0 ns) (length ns)))
