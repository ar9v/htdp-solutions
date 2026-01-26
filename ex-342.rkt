#lang htdp/isl+

(require htdp/dir)
(require 2htdp/abstraction)

;;; Design `find`. The function consumes a directory `d` and a file name `f`. If
;;; `(find? d f) is #true, `find` produces a path to a file with name `f`; otherwise
;;; it produces #false

(define test-dir (create-dir "./ex-338"))

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

; find: Dir String -> [Maybe Path]
; Returns the path to a file named `fname` in `dir`, if it exists; otherwise returns false.
(check-expect (find test-dir "does-not-exist.txt") #false)
(check-expect (find test-dir "ttt.txt") '("ex-338" "ttt.txt"))
(check-expect (find test-dir "last-reader.txt") '("ex-338" "dummy-dir" "last-reader.txt"))
(check-expect (find TS "read!") '("TS" "read!"))
(check-expect (find TS "hang") '("TS" "Libs" "Code" "hang"))
(define (find d f)
  (if (member? f (map file-name (dir-files d)))
      (list (dir-name d) f)
      (local [(define result (for/or [(dir (dir-dirs d))] (find dir f)))]
        (if (not (false? result))
            (cons (dir-name d) result)
            #false))))

;;; Challenge:
;;; The `find` function discovers only one of the two files named `read!` in figure 123.
;;; Design `find-all`, which generalizes `find` and produces the lilst of all paths that
;;; list to `f` in `d`. What should `find-all` produce when `(find? d f)` is #false? Is
;;; this part of the problem really a challenge compared to the basic problem?

; find-all: Dir String -> [List-of Path]
; Returns all of the paths to a given filename `f` in `d`, if any. Otherwise, returns
; the empty list.
(check-expect (find-all TS "foo") '())
(check-expect (find-all TS "hang") '(("TS" "Libs" "Code" "hang")))
(check-expect (find-all TS "read!") '(("TS" "read!") ("TS" "Libs" "Docs" "read!")))
(define (find-all d f)
  (local [(define dname (dir-name d))
          (define in-rest
            (map (λ (path) (cons dname path))
                 (foldr append '() (for/list [(dir (dir-dirs d))] (find-all dir f)))))]
    (if (member? f (map file-name (dir-files d)))
        (cons (list dname f) in-rest)
        in-rest)))
