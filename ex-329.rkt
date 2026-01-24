#lang htdp/isl+

;;; How many times does a file name `read!` occur in the directory tree `TS`?
;;;
;;; A: 2

;;; Can you describe the path from the root directory to the occurrences?
;;;
;;; A:
;;; '(TS read!)
;;; '(TS Libs Docs read!)

;;; What is the total size of all the files in the tree?
;;;
;;; A:
;;; (+ 10 99 52 17 19 8 2) == 207

;;; What is the total size of the directory if each directory node has size 1?
;;;
;;; A: 207 + 5 = 212

;;; How many levels of directories does it contain?
;;;
;;; A: Depends on whether the root counts! Could be 2 or 3.
