#lang htdp/bsl

;;; Provide a structure type and a data definition for representing three letter words.
;;; A word consists of lowercase letters, represented with 1Strings "a" through "z" plus
;;; #false.
;;;
;;; NOTE: This exercise is a part of the design of a hangman game; see Exercise 396.

; a Word is a structure
;   (make-3-letter-word Letter Letter Letter)
;
; interpretation:
; (make-3-letter-word first second third) represents a three letter word comprised of
; lowercase letters "a" through "z" or #false
(define-struct 3-letter-word [first second third])

; a Letter is one of
; -- a 1String, which is in the range of lowercase characters "a" through "z"
; -- False
