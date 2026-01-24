#lang htdp/isl+

;;; Translate the directory tree in figure 123 into a data representation according to
;;; model 2.

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
