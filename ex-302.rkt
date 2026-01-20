#lang htdp/isl+

;;; Recall that each occurrence of a variable receives its value from its binding
;;; occurrence. Consider the following definition:

; (define x (cons 1 x))

;;; Where is the shaded occurrence of `x` bound? Since the definition is a constant
;;; definition and not a function definition, we need to evaluate the right-hand side
;;; immediately. What should be the value of the right hand side according to our rules?
;;;
;;; A:
;;; This will result in an error, since `x` is unbound. Even if it were somehow bound,
;;; (i.e. if we simply don't have the full context for this exercise), ISL(+) disallows
;;; redefining it.
