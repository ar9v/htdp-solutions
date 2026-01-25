#lang htdp/isl+

(require 2htdp/abstraction)

;;; Use `List-of` to simplify the data definition Dir.v3. Then use ISL+'s list processing
;;; functions from figure 95 and 96 to simplify the function definition(s) for the solution
;;; of exercise 336.

(define-struct file [name size content])
; A File is a structure:
;  (make-file String N String)

(define-struct dir [name dirs files])
; A Dir is a structure:
;  (make-dir String [List-of Dir] [List-of File])

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
