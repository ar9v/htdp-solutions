#lang htdp/isl+

(require htdp/dir)

;;; Redesign `find-all` from exercise 342 using `ls-R` from exercise 343. This is design
;;; by composition, and if you solved the challenge part of exercise 342 your new function
;;; can find directories, too.

(define a-date (make-date 2026 1 25 21 23 0))

(define part1 (make-file "part1" 0 a-date ""))
(define part2 (make-file "part2" 0 a-date ""))
(define part3 (make-file "part3" 0 a-date ""))

(define hang (make-file "hang" 0 a-date ""))
(define draw (make-file "draw" 0 a-date ""))

(define ts-read (make-file "read!" 0 a-date ""))
(define docs-read (make-file "read!" 0 a-date ""))

(define Docs (make-dir "Docs"
                       '()
                       (list docs-read)))

(define Code (make-dir "Code"
                       '()
                       (list hang draw)))

(define Libs (make-dir "Libs"
                       (list Code Docs)
                       '()))

(define Text (make-dir "Text"
                       '()
                       (list part1 part2 part3)))

(define TS (make-dir "TS"
                     (list Text Libs)
                     (list ts-read)))

; find-all: Dir String -> [List-of Path]
; Finds all paths to filename `fname` in `dir` if it exists; returns the empty list
; otherwise
(check-expect (find-all TS "foo") '())
(check-expect (find-all TS "hang") '(("TS" "Libs" "Code" "hang")))
(check-expect (find-all TS "read!") '(("TS" "read!") ("TS" "Libs" "Docs" "read!")))
(define (find-all d fname)
  (filter (λ (path) (member? fname path)) (ls-R d)))

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
