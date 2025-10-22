#lang htdp/bsl

;; Design the function `string-last`, which extracts the last character from a non-empty
;; string.

;; A:

;;; 1. Data Definitions
;;;
;;; A NonEmptyString is a string for which (string-length s) > 0
;;; A 1String is a string for which (string-length s) === s

;;; 2. Signature, Statement of Purpose, Header
;;;
;;; string-last: NonEmptyString -> 1String
;;; Returns the last character of `s`, the given non-empty string
;;;
;;; (define (string-last s) s)

;;; 3. Functional Examples
;;;
;;; given: "a", expect: "a"
;;; given: "31", expect: "1"
;;; given: "c0rr3ct h0rs3, b4tt3ry 5t4pl3!", expect: "!"

;;; 4. Inventory/Function Template
;;;
;;; (define (string-last s) (... s ...))

;;; 5. Code/Fill out template

;;; string-last: NonEmptyString -> 1String
;;; Returns the last character of `s`, the given non-empty string
;;;
;;; given: "a", expect: "a"
;;; given: "31", expect: "1"
;;; given: "c0rr3ct h0rs3, b4tt3ry 5t4pl3!", expect: "!"
(define (string-last s)
  (string-ith s (sub1 (string-length s))))

;;; 6. Tests
(and
 (string=? (string-last "a") "a")
 (string=? (string-last "31") "1")
 (string=? (string-last "c0rr3ct h0rs3, b4tt3ry 5t4pl3!") "!"))
