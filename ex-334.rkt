#lang htdp/isl+

;;; Show how to equip a directory with two more attributes: size and readability. The
;;; former measures how much space the directory itself (as opposed to its content)
;;; consumes; the latter specifies whether anyone else beside the user may browse the
;;; content of the directory.

(define-struct dir [name content size readable?])
; A Dir is a structure
;   (make-dir String LOFD Number Boolean)
;
; interpretation: (make-dir name contents size readable?) represents a directory named
; `name`, which contains files or other directories (`contents`), which has a given
; `size` and which may be readable by anyone.
;
; A LOFD is one of
; -- '()
; -- (cons File LOFD)
; -- (cons Dir LOFD)
;
; a File is a String

(define ts
  (make-dir
   "TS"
   (list "read!"
         (make-dir "Text" '("part1" "part2" "part3") 1 #true)
         (make-dir
          "Libs"
          (list (make-dir "Code" '("hang" "draw") 1 #false)
                (make-dir "Docs" '("read!") 1 #true))
          1
          #true))
   1
   #true))
