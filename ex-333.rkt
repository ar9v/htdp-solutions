#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design the function `how-many`, which determines how many files a given Dir.v2
;;; contains. Exercise 332 provides you with data examples.

(define-struct dir [name content])
; A Dir.v2 is a structure:
;   (make-dir String LOFD)
;
; An LOFD (short for list of files and directories) is one of:
; -- '()
; -- (cons File.v2 LOFD)
; -- (cons Dir.v2 LOFD)
;
; A File.v2 is a String.

(define ts
  (make-dir
   "TS"
   (list "read!"
         (make-dir "Text" '("part1" "part2" "part3"))
         (make-dir
          "Libs"
          (list (make-dir "Code" '("hang" "draw"))
                (make-dir "Docs" '("read!")))))))

; how-many: Dir.v2 -> N
; Counts `dir`'s files
(check-expect (how-many (make-dir "Empty" '())) 0)
(check-expect (how-many ts) 7)
(define (how-many dir)
  (for/sum [(i (dir-content dir))] (if (string? i) 1 (how-many i))))

;;; Compare your result with that of exercise 331.
;;;
;;; A:
;;; It's slightly more complicated, since we know have to make sure we select the
;;; directory's contents; but it's mostly the same. It makes sense since this function
;;; doesn't really asks for/is concerned with the extra data that the definition adds.
