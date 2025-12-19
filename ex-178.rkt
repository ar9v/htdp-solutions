#lang htdp/bsl

;;; Explain why the template for `editor-kh` deals with `\t` and `\r` before it checks for
;;; strings of length 1.

;; Template:
;; (define (editor-kh ed k)
;;   (cond
;;     [(key=? k "left") ...]
;;     [(key=? k "right") ...]
;;     [(key=? k "\b") ...]
;;     [(key=? k "\t") ...]
;;     [(key=? k "\r") ...]
;;     [(= (string-length k) 1) ...]
;;     [else ...]))


;;; A:
;;;
;;; Because they are considered 1Strings as well, but we don't want to treat them the
;;; same way as other 1Strings.
