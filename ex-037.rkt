#lang htdp/bsl

;; Design the function `string-rest`, which produces a string like the given one with
;; the first character removed.

;;; A:

;;; 1. Data Definition
;;;
;;; Again, not much to do here, since strings are primitives

;;; 2. Signature, Statement of Purpose, Header
;;;
;;; string-rest: String -> String
;;; Given a string `s`, returns a string which is `s` except for the first character.
;;; If `s` is empty, `s` is returned.
;;;
;;; (define (string-rest s) s)

;;; 3. Functional Examples
;;;
;;; given: "abc", expect: "bc"
;;; given: "b", expect: ""
;;; given: "", expect: ""

;;; 4. Inventory/Function Template
;;;
;;; (define (string-rest s) (... s ...))

;;; 5. Code/Put it Together
;;;
;;; string-rest: String -> String
;;; Given a string `s`, returns a string which is `s` except for the first character.
;;; If `s` is empty, `s` is returned.
;;;
;;; given: "abc", expect: "bc"
;;; given: "b", expect: ""
;;; given: "", expect: ""
(define (string-rest s)
  (if (string=? s "")
      ""
      (substring s 1)))

;;; 6. Tests
(and
 (string=? (string-rest "abc") "bc")
 (string=? (string-rest "b") "")
 (string=? (string-rest "") ""))
