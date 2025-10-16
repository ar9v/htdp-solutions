#lang htdp/bsl

;; Define the function `string-insert`, which consumes a string `str` plus a number `i`
;; and inserts "_" at the ith position of `str`. Assume `i` is a number between 0 and
;; the length of the given string (inclusive). See exercise 3 for ideas.
(define (string-insert str i)
  (string-append (substring str 0 i)
                 "_"
                 (substring str i)))

(string=? "_hello" (string-insert "hello" 0))
(string=? "he_llo" (string-insert "hello" 2))
(string=? "hello_" (string-insert "hello" 5))

;; Ponder how `string-insert` copes with "".
;;
;; Answer:
;; Since we assume `i` is between 0 and (string-length str) inclusive, then `string-insert`
;; could only take 0 as a value for `i`, which is handled appropriately (it yields "_")
(string=? "_" (string-insert "" 0))
