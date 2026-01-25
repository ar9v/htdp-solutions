#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design the function `how-many`, which determines how many files a given Dir.v3
;;; contains. Exercise 335 provides you with data examples. Compare your result with
;;; that of exercise 333.

(define-struct file [name size content])
; A File is a structure:
;  (make-file String N String)

(define-struct dir [name dirs files])
; A Dir is a structure:
;  (make-dir String Dir* File*)
;
; A Dir* is one of:
; -- '()
; -- (cons Dir Dir*)
;
; A File* is one of:
; -- '()
; -- (cons File File*)

(define ts
  (make-dir "TS"
            (list (make-dir "Text"
                            '()
                            (list (make-file "part1" 99 "")
                                  (make-file "part2" 52 "")
                                  (make-file "part3" 17 "")))
                  (make-dir "Libs"
                            (list (make-dir "Code"
                                            '()
                                            (list (make-file "hang" 8 "")
                                                  (make-file "draw" 2 "")))
                                  (make-dir "Docs"
                                            '()
                                            (list (make-file "read!" 19 ""))))
                            '()))
            (list (make-file "read!" 10 ""))))

; how-many?: Dir -> N
; Counts the files in `dir`
(check-expect (how-many (make-dir "Empty" '() '())) 0)
(check-expect (how-many ts) 7)
(define (how-many dir)
  (+ (length (dir-files dir))
     (for/sum [(d (dir-dirs dir))] (how-many d))))

;;; Given the complexity of the data definition, contemplate how anyone can design
;;; correct functions. Why are you confident that `how-many` produces correct results?
;;;
;;; A:
;;; It is hard to claim that one can be "confident" or "sure" one is producing correct
;;; results, since really the only claim to confidence would be a proof. But we can
;;; have _some_ degree of confidence, in terms of what the book has taught us, by the
;;; way of tests and, chiefly, by way of having applied the design recipe. That is to say:
;;; the data definition is complex, but only inasmuch as it is built of simpler data
;;; definitions, which we've extensively studied and for which we've teased out
;;; working abstractions.
