#lang htdp/bsl

;;; Provide a data definition for representing lists of Boolean values. The class contains
;;; all arbitrarily long lists of Booleans.

;;; a List-of-Booleans is
;;; --- '()
;;; --- (cons Boolean List-of-Booleans)

(define ex-1 '())
(define ex-2 (cons #true '()))
(define ex-3 (cons #false '()))
(define ex-4 (cons #true (cons #false (cons #true '()))))
