#lang htdp/bsl

;; Design the function `string-first`, which extracts the first character from a non-empty
;; string.

;; A:

;;; 1. Data Definitions
;;;
;;; A NonEmptyString is a String for which (string-length) > 0
;;; A 1String is a String where (string-length) === 1

;;; 2. Signature, Statement of Purpose and Function Header
;;;
;;; string-first: NonEmptyString -> 1String
;;; Returns the first character of `s`, the given non-empty string.
;;;
;;; (define (string-first s) s)

;;; 3. Functional Examples
;;;
;;; given: (string-first "a"),                              expect: "a"
;;; given: (string-first "31"),                             expect: "3"
;;; given: (string-first "c0rr3ct! h0r53 b4tt3ry, 5t4pl3"), expect: "c"

;;; 4. Inventory
;;;
;;; (define (string-first s) (... s ...))

;;; 5. Code/Fill out the template
;;;
;;; string-first: NonEmptyString -> 1String
;;; Returns the first character of `s`, the given non-empty string.
;;;
;;; given: (string-first "a"),                              expect: "a"
;;; given: (string-first "31"),                             expect: "3"
;;; given: (string-first "c0rr3ct! h0r53 b4tt3ry, 5t4pl3"), expect: "c"
(define (string-first s)
  (string-ith s 0))

;;; 6. Tests
(and (string=? (string-first "a") "a")
     (string=? (string-first "31") "3")
     (string=? (string-first "c0rr3ct! h0r53 b4tt3ry, 5t4pl3") "c"))
