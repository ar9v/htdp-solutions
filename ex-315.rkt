#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design the function `average-age`. It consumes a family forest and a year (N). From
;;; this data, it produces the average age of all `child` instances in the forest.
;;;
;;; NOTE: If the trees in this forest overlap, the result isn't a true average because
;;; some people contribute more than others. For this exercise, act as if the trees
;;; don't overlap

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

; A Family Forest (FF) is a [List-of FT]
(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))

; average-age: FF N -> N
; produces the average age of all `child` instances in the forest.
(check-error (average-age '() 2000))
(check-expect (average-age ff1 2026) (average (ages/ff ff1 2026)))
(check-expect (average-age ff2 2026) (average (ages/ff ff2 2026)))
(define (average-age ff current-year)
  (average (ages/ff ff current-year)))

; ages/ff: FF N -> [List-of N]
; Produces a list of all ages in the forest, given a `year` as reference
(check-expect (ages/ff ff1 2026) (list (- 2026 1926) (- 2026 1926)))
(check-expect (ages/ff ff2 2026)
              (list (- 2026 1966) (- 2026 1965) (- 2026 1926) (- 2026 1926)))
(define (ages/ff ff year)
  (match ff
    ['() '()]
    [(cons ft rst) (append (ages/ft ft year) (ages/ff rst year))]))

; ages/ft: FT N -> [List-of N]
; Given an FT and a reference `year`, produces a list of ages of all
; members of the family tree
(check-expect (ages/ft Carl 2026) (map (age 2026) '(1926)))
(check-expect (ages/ft Dave 2026) (map (age 2026) '(1955 1926 1926)))
(check-expect (ages/ft Gustav 2026) (map (age 2026) '(1988 1966 1965 1926 1926)))
(define (ages/ft ft year) (map (age year) (dates/ft ft)))

; dates/ft: FT -> [List-of N]
; Produces a list of all dates of birth (i.e. (child-date c)) of the
; elements in tree `ft`
(check-expect (dates/ft Carl) '(1926))
(check-expect (dates/ft Dave) '(1955 1926 1926))
(check-expect (dates/ft Gustav) '(1988 1966 1965 1926 1926))
(define (dates/ft ft)
  (match ft
    [(no-parent) '()]
    [(child f m _n dob _e)
     (append (list dob) (dates/ft f) (dates/ft m))]))

; age: N -> [N -> N]
; Returns a function that computes an age with respect to `year`, once its given a
; date of birth
(define (age year) (λ (dob) (- year dob)))

; average: [NonEmptyList-of N] -> N
; Computes the average of the given list of numbers
(define (average ns) (/ (foldr + 0 ns) (length ns)))
