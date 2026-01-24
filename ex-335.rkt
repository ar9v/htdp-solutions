#lang htdp/isl+

;;; Translate the directory tree in figure 123 into a data representation according to
;;; model 3. Use "" for the content of files.

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
