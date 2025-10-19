#lang htdp/bsl

(require 2htdp/batch-io)

;; Recall the `letter` program from chapter 2.3.
(define (letter fst lst signature-name)
  (string-append
   (opening fst)
   "\n\n"
   (body fst lst)
   "\n\n"
   (closing signature-name)))

(define (opening fst)
  (string-append "Dear " fst ","))

(define (body fst lst)
  (string-append
   "We have discovered that all people with the" "\n"
   "last name " lst " have won our lottery. So, " "\n"
   fst ", " "hurry and pick up your prize."))

(define (closing signature-name)
  (string-append
   "Sincerely,"
   "\n\n"
   signature-name
   "\n"))

;; Here is how to launch the program
;; and have it write its output to the interactions area:

(write-file
 'stdout
 (letter "Matthew" "Fisler" "Felleisen"))


;; Of course, programs are useful because you can launch them from many different inputs.
;; Run `letter` on three inputs of your choice.
;;
;; Here is a letter-writing batch program that reads names from three files and writes a
;; letter to one:
(define (main in-fst in-lst in-signature out)
  (write-file out
              (letter (read-file in-fst)
                      (read-file in-lst)
                      (read-file in-signature))))

;; The function consumes four strings: the first three are the names of input files and
;; the last one serves as an output file. It uses the first three to read one string each
;; from the three named files, hands these strings to `letter`, and eventually writes the
;; result of this function into the file named `out`, the fourth argument to `main`.
;;
;; Create appropriate files, launch `main`, and check whether it delivers the expected
;; letter in a given file.
(main "ex-031-fst.txt" "ex-031-lst.txt" "ex-031-signature.txt" "ex-031-out.txt")
(write-file 'stdout (read-file "ex-031-out.txt"))
