#lang htdp/bsl

;; Why is `(string=? "resting" x)` **incorrect** as the first condition of `show`?
;; Conversely, formulate a completely accurate condition, that is, a Boolean expression
;; that evaluates to `#true` precisely when `x` belongs to the first sub-class of `LRCD`

;;; Answer:
;;;
;;; It is wrong because LRCD can be a string ("resting") but _also_ numbers! So if we used
;;; `(string=? "resting" x)`, the program would fail since `string=?` expects two strings.
;;;
;;; What we'd want is actually `(and (string? x) (string=? "resting" x))`, to make sure
;;; we only call `string=?` when `x` is a string.
