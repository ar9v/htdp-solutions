#lang htdp/bsl

;;; Will `sum` and `how-many` work for NEList-of-temperatures even though they are designed
;;; for inputs from List-of-temperatures? If you think they don't work, provide
;;; counter-examples. If you think they would, explain why.

;;; A:
;;; They will work, because they are defined for a domain that is a superset of
;;; NEList-of-temperatures! Meaning: all while not all List-of-temperatures are
;;; NEList-of-temperatures, all NEList-of-temperatures /are/ List-of-temperatures.
