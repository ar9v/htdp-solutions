#lang htdp/isl+

(require 2htdp/abstraction)

(define-struct no-parent [])
(define NP (make-no-parent))
(define-struct child [father mother name date eyes])
; A FT is one of
; -- NP
; -- (make-child FT FT String N String)

;;; Suppose we need the function `blue-eyed-ancestor?`, which is like `blue-eyed-child?`,
;;; but responds with #true only when a proper ancestor, not the given `child` itself,
;;; has blue eyes.
;;;
;;; Although the goals clearly differ, the signatures are the same:

; blue-eyed-ancestor?: FT -> Boolean
; True if an ancestor of `an-ftree` has blue eyes (but not `an-ftree` itself)
;
;; (define (blue-eyed-ancestor? an-ftree)
;;   #false)

;;; To appreciate the difference, we take a look at Eva:

; (check-expect (blue-eyed-child? Eva) #true)

;;; Eva is blue-eyed, but has no blue-eyed ancestor. Hence,

; (check-expect (blue-eyed-ancestor? Eva) #false)

;;; In contrast, Gustav is Eva’s son and does have a blue-eyed ancestor:

; (check-expect (blue-eyed-ancestor? Gustav) #true)

;;; Now suppose a friend comes up with this solution:

;; (define (blue-eyed-ancestor? an-ftree)
;;   (cond
;;     [(no-parent? an-ftree) #false]
;;     [else
;;      (or
;;       (blue-eyed-ancestor?
;;        (child-father an-ftree))
;;       (blue-eyed-ancestor?
;;        (child-mother an-ftree)))]))

;;; Explain why this function fails one of its tests.
;;; What is the result of `(blue-eyed-ancestor? A)` no matter which A you choose?
;;;
;;; A: This fails because `blue-eyed-ancestor?` always returns `#false` (!) There is no
;;;    expression in its body that could possibly evaluate to true; it just keeps
;;;    traversing the tree until it hits the base case.

;;; Can you fix your friend’s solution?
;;;
;;; Yes. We can leverage `blue-eyed-child?` by calling it with `an-ftree`'s parents.

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

; blue-eyed-child?: FT -> Boolean
; does an-ftree contain a child structure with "blue" in the eyes field
(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
(define (blue-eyed-child? an-ftree)
  (cond [(no-parent? an-ftree) #false]
        [else (or (string=? (child-eyes an-ftree) "blue")
                  (blue-eyed-child? (child-father an-ftree))
                  (blue-eyed-child? (child-mother an-ftree)))]))

; blue-eyed-ancestor?: FT -> Boolean
; True if an ancestor of `an-ftree` has blue eyes (but not `an-ftree` itself)
(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)
(define (blue-eyed-ancestor? an-ftree)
  (match an-ftree
    [(no-parent) #false]
    [(child f m _n _d _e) (or (blue-eyed-child? f) (blue-eyed-child? m))]))
