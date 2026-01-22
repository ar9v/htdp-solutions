#lang htdp/isl+

;;; Reformulate the data definition for FF with the List-of abstraction. Now do the same
;;; for the `blue-eyed-child-in-forest?` function. Finally, define
;;; `blue-eyed-child-in-forest?` using one of the list abstractions from the preceding
;;; chapter.

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
(define ff3 (list Fred Eva Carl))

; blue-eyed-child?: FT -> Boolean
; does an-ftree contain a child structure with "blue" in the eyes field
(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
(define (blue-eyed-child? an-ftree)
  (cond [(no-parent? an-ftree) #false]
        [else (or (string=? (child-eyes an-ftree) "blue")
                  (blue-eyed-child? (child-father an-ftree))
                  (blue-eyed-child? (child-mother an-ftree)))]))

; blue-eyed-child-in-forest?: [List-of FT] -> Boolean
; True if there's any child in this forest whose eyes are blue
(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)
(define (blue-eyed-child-in-forest? ff)
  (ormap blue-eyed-child? ff))
