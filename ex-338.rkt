#lang htdp/isl+

(require htdp/dir)
(require 2htdp/abstraction)

;;; Use `create-dir` to turn some of your directories into ISL+ data representations. Then
;;; use `how-many` from exercise 336 to count how many files they contain. Why are you
;;; confident that `how-many` produces correct results for these directories?
;;;
;;; A: In a broad sense, because I can see those directories and confirm that the function
;;; produces what I expect myself. For cases in which this is not a trivial thing to do
;;; we can rely on time-tested programs that do the same thing and compare the results.
;;; (e.g. `find -type f | wc -l`)

; how-many?: Dir -> N
; Counts the files in `dir`
(check-expect (how-many (make-dir "Empty" '() '())) 0)
(check-expect (how-many (create-dir "./ex-338/")) 3)
(define (how-many dir)
  (+ (length (dir-files dir))
     (for/sum [(d (dir-dirs dir))] (how-many d))))
