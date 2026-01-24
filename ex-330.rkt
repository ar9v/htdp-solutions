#lang htdp/isl+

;;; Translate the directory tree in figure 123 into a data representation according to
;;; model 1

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
