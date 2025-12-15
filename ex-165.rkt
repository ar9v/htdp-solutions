#lang htdp/bsl

;;; Design the function `subst-robot`, which consumes a list of toy descriptions
;;; (one-word strings) and replaces all occurrences of "robot" with "r2d2"; all other
;;; descriptions remain the same.
;;;
;;; Generalize `subst-robot` to `substitute`. The latter consumes two strings, called
;;; `new` and `old`, and a list of strings. It produces a new list of strings by
;;; substituting all occurrences of `old` with `new`.

; subst-robot: List-of-1Word -> List-of-1Word
; Replaces all occurrences of "robot" with "r2d2"
(check-expect (subst-robot '()) '())
(check-expect (subst-robot (cons "train" (cons "ball" '())))
              (cons "train" (cons "ball" '())))
(check-expect (subst-robot (cons "train" (cons "robot"  (cons "ball" '()))))
              (cons "train" (cons "r2d2" (cons "ball" '()))))
(define (subst-robot low)
  (cond [(empty? low) '()]
        [(cons? low)
         (cons
          (if (string=? (first low) "robot") "r2d2" (first low))
          (subst-robot (rest low)))]))

; substitute: String String List-of-strings -> List-of-strings
; Replaces all occurrences of `old` with `new` in `los`
(check-expect (substitute "foo" "bar" '()) '())
(check-expect (substitute "robot" "r2d2"
                          (cons "train" (cons "robot"  (cons "ball" (cons "robot" '())))))
              (cons "train" (cons "r2d2" (cons "ball" (cons "r2d2" '())))))
(check-expect (substitute "foo" "bar" (cons "baz" '())) (cons "baz" '()))
(define (substitute old new los)
  (cond [(empty? los) '()]
        [(cons? los)
         (cons
          (if (string=? (first los) old) new (first los))
          (substitute old new (rest los)))]))
