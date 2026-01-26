#lang htdp/isl+

(require htdp/dir)

;;; Design the function `ls`, which lists the names of all files and directories in a
;;; given Dir.

; ls: Dir -> [List-of String]
; Produces a list of all the (immediate) file and subdirectory names in `dir`.
(check-expect (ls (create-dir "./ex-338")) '("dummy-dir" "builders.txt" "ttt.txt"))
(define (ls dir)
  (append (map dir-name (dir-dirs dir))
          (map file-name (dir-files dir))))
