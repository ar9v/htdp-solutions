#lang htdp/bsl

;; Design the function `string-remove-last`, which produces a string like the given one
;; with the *last* character removed.

;;; A:

;;; 1. Data Definitions
;;;
;;; ibid.

;;; 2. Signature, Statement of Purpose, Header
;;;
;;; string-remove-last: String -> String
;;; Given a String `s`, return a new string that is `s` w/o its last character.
;;; If `s` is empty, return `s`.
;;;
;;; (define (string-remove-last s) s)

;;; 3. Functional Examples
;;;
;;; given: "abc", expect: "ab"
;;; given: "b", expect: ""
;;; given: "", expect: ""

;;; 4. Take Inventory/Function Template
;;;
;;; (define (string-remove-last s) (... s ...))

;;; 5. Code/Put it Together
;;;
;;; string-remove-last: String -> String
;;; Given a String `s`, return a new string that is `s` w/o its last character.
;;; If `s` is empty, return `s`.
;;;
;;; given: "abc", expect: "ab"
;;; given: "b", expect: ""
;;; given: "", expect: ""
(define (string-remove-last s)
  (if (string=? s "")
      ""
      (substring s 0 (sub1 (string-length s)))))

;;; 6. Tests
(and
 (string=? (string-remove-last "abc") "ab")
 (string=? (string-remove-last "b") "")
 (string=? (string-remove-last "") ""))
