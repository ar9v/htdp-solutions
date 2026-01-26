#lang htdp/isl+

(require htdp/dir)
(require 2htdp/abstraction)

;;; Design the function `ls-R`, which lists the paths to *all* files contained in a given
;;; Dir.

(define a-date (make-date 2026 1 25 21 23 0))

(define part1 (make-file "part1" 0 a-date ""))
(define part2 (make-file "part2" 0 a-date ""))
(define part3 (make-file "part3" 0 a-date ""))
(define hang (make-file "hang" 0 a-date ""))
(define draw (make-file "draw" 0 a-date ""))
(define ts-read (make-file "read!" 0 a-date ""))
(define docs-read (make-file "read!" 0 a-date ""))

(define Docs (make-dir "Docs" '() (list docs-read)))
(define Code (make-dir "Code" '() (list hang draw)))
(define Libs (make-dir "Libs" (list Code Docs) '()))
(define Text (make-dir "Text" '() (list part1 part2 part3)))
(define TS (make-dir "TS" (list Text Libs) (list ts-read)))

; ls-R: Dir -> [List-of Path]
; Lists all paths in a dir, recursively
(check-expect (ls-R TS)
              '(("TS" "read!")
                ("TS" "Text" "part1")
                ("TS" "Text" "part2")
                ("TS" "Text" "part3")
                ("TS" "Libs" "Code" "hang")
                ("TS" "Libs" "Code" "draw")
                ("TS" "Libs" "Docs" "read!")))
(define (ls-R dir)
  (local [(define dirname (dir-name dir))]
    (append
     (map (λ (f) (list dirname (file-name f))) (dir-files dir))
     (map (λ (path) (cons dirname path))
          (foldr (λ (d paths) (append (ls-R d) paths)) '() (dir-dirs dir))))))
