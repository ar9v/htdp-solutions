#lang htdp/bsl

;;; Design the function `compare-word`. The function consumes two three-letter words
;;; (see Exercise 78). It produces a word that indicates where the given ones agree and
;;; disagree. The function retains the content of the structure fields if the two agree;
;;; otherwise it places `#false` in the field of the resulting word.
;;;
;;; HINT: The exercise mentions two tasks: the comparison of words and the comparison of
;;; "letters".

; a Word is a structure
;   (make-3-letter-word Letter Letter Letter)
;
; interpretation:
; (make-3-letter-word first second third) represents a three letter word comprised of
; lowercase letters "a" through "z" or #false
(define-struct word [first second third])

(define ant-word (make-word "a" "n" "t"))
(define apt-word (make-word "a" "p" "t"))
(define cat-word (make-word "c" "a" "t"))
(define all-false (make-word #false #false #false))

; a Letter is one of
; -- a 1String, which is in the range of lowercase characters "a" through "z"
; -- False


; compare-word: Word Word -> Word
; Produces a new Word, where the value of each field is either the field's value itself
; if it matches for w1 and w2, or #false if it doesn't.
(check-expect (compare-word ant-word ant-word) ant-word)
(check-expect (compare-word ant-word apt-word) (make-word "a" #false "t"))
(check-expect (compare-word cat-word all-false) all-false)
(define (compare-word w1 w2)
  (make-word (compare-letter (word-first w1) (word-first w2))
             (compare-letter (word-second w1) (word-second w2))
             (compare-letter (word-third w1) (word-third w2))))

; compare-letter: Letter Letter -> Letter
; Compares l1 and l2; if they are the same, it returns l1; #false otherwise
(check-expect (compare-letter "a" "a") "a")
(check-expect (compare-letter "n" "p") #false)
(check-expect (compare-letter "c" #false) #false)
(define (compare-letter l1 l2)
  (if (equal? l1 l2) l1 #false))

;; NOTE: There's a tiny bit of cheating/cleverness involved in `compare-letter`. I think
;; it's fair-game because it was introduced in Figure 27.
;;
;; One should only do this after refactoring a `cond` expression that clearly addresses
;; each of Letter's possible states. And even then, it's technically more relaxed still
;; i.e. we assume we're being given Letters. /CAVEAT EMPTOR
