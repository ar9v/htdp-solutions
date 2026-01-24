#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design the function `how-many`, which determines how many files a given Dir.v1
;;; contains. Remember to follow the design recipe; exercise 330 provides you with data
;;; examples.

; A Dir.v1 (short for directory) is one of:
; -- '()
; -- (cons File.v1 Dir.v1)
; -- (cons Dir.v1 Dir.v1)
;
; A File.v1 is a String.

(define ts
  '("read!"
    ("part1" "part2" "part3")
    (("hang" "draw") ("read!"))))

; how-many: Dir.v1 -> N
; Counts `dir`'s files
(check-expect (how-many '()) 0)
(check-expect (how-many ts) 7)
(define (how-many dir)
  (for/sum [(d dir)] (if (string? d) 1 (how-many d))))
