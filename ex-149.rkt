#lang htdp/bsl

;;; Does `copier` function properly when you apply it to a natural number and a Boolean
;;; or an image? Or do you have to design another function? Read part III for the answer.

;;; A: It works for anything! Since `cons` can construct lists of any type.


;;; An alternative definition of `copier` might use `else`:

(define (copier n s)
  (cond [(zero? n) '()]
        [(positive? n) (cons s (copier (sub1 n) s))]))

(define (copier.v2 n s)
  (cond [(zero? n) '()]
        [else (cons s (copier.v2 (sub1 n) s))]))

;;; How do `copier` and `copier.v2` behave when you apply them to 0.1 and "x"? Explain.
;;; Use DrRacket's stepper to confirm your explanation

;;; A:
;;;
;;; `copier` will
;;;   -- Fail for 0.1 after 1 iteration, since it'll call `(copier -0.9 s)`, and then the
;;;      `cond` will not be exhaustive (-0.9 is neither `zero?` nor `positive?`)
;;;
;;; `copier.v2` will
;;;   -- Loop indefinitely (!) for 0.1, since we'll skip over 0
