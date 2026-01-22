#lang htdp/isl+

(require 2htdp/abstraction)

;;; Develop the function `eye-colors`, which consumes a family tree and produces a list
;;; of all eye colors in the tree. An eye color may occur more than once in the resulting
;;; list. HINT: use `append` to concatenate the lists resulting from the recursive calls.

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

; eye-colors: FT -> [List-of String]
; Produces all the eye colors in the family tree
(check-expect (eye-colors Gustav) '("brown" "pink" "blue" "green" "green"))
(define (eye-colors ft)
  (match ft
    [(no-parent) '()]
    [(child f m _n _d eye-color)
     (append (list eye-color)
             (eye-colors f)
             (eye-colors m))]))
