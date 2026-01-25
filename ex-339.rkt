#lang htdp/isl+

(require htdp/dir)
(require 2htdp/abstraction)

;;; Design `find?`. The function consumes a Dir and a file name and determines whether or
;;; not a file with this name occurs in the directory tree.

; find?: Dir String -> Boolean
; Does File with name `fname` exist in Dir `dir`?
(check-expect (find? (create-dir ".") "ttt.txt") #true)
(check-expect (find? (create-dir ".") "ex-338.rkt") #true)
(check-expect (find? (create-dir ".") "builders.txt") #true)
(check-expect (find? (create-dir ".") "foo.txt") #false)
(define (find? dir fname)
  (or (member? fname (map file-name (dir-files dir)))
      (for/or [(d (dir-dirs dir))] (find? d fname))))
