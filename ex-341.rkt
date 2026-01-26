#lang htdp/isl+

(require htdp/dir)
(require 2htdp/abstraction)

;;; Design `du`, a function that consumes a Dir and computes the total size of all the
;;; files in the entire directory tree. Assume that storing a directory in a Dir structure
;;; costs 1 file storage unit. In the real world, a directory is basically a special file,
;;; and its size depends on how large its associated directory is.

; du: Dir -> Number
; Returns the total size of all the files in the directory tree. (`du -abc`)
(check-expect (du (create-dir "./ex-338")) (+ 1574 1188 184 1))
(define (du dir)
  (+ (for/sum [(f (dir-files dir))] (file-size f))
     (for/sum [(d (dir-dirs dir))] (add1 (du d)))))
