#lang htdp/bsl

;;; Formulate a data definition for the above `phone` structure type definition that
;;; accommodates the given examples

;;; A Phone is a structure
;;;   (make-phone Number String)
;;;
;;; interpretation:
;;;   A phone number with an area code `area`, and a formatted string `number`
(define-struct phone [area number])

;;; Next formulate a data definition for phone numbers using this structure

;;; A Phone# is a structure
;;;   (make-phone# Area Switch NNumber)
;;;
;;; interpretation:
;;;   A phone number with an area code `area`, the neighborhood exchange number `switch`
;;;   and a neighborhood line number `num`.
;;;
;;; Area is a Number, the area code for the number, which may be [200, 999]
;;; Switch is a Number, the neighborhood exchange number, which may be [200, 999]
;;; NNumber is a String of the form "XXXX", the line number, where each X is a character
;;;   in the range [0, 9]*
(define-struct phone# [area switch num])

;;; Historically, the first three digits make up the area code, the next three the
;;; code for the phone switch (exchange) of your neighborhood, and the last four the
;;; phone with respect to the neighborhood. Describe the content of the three fields
;;; as precisely as possible with intervals.


;;; * Note: This is a lax interpretation of NANP. Area and Switch, e.g. cannot be X11
;;; (e.g. 911), amongst other things. But you get the idea.
