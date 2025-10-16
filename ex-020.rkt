#lang htdp/bsl

;; Define the function `string-delete`, which consumes a string plus a number `i` and
;; deletes the ith position from `str`. Assume `i` is a number between 0 (inclusive) and
;; the length of the given string (exclusive). See exercise 4 for ideas.
(define (string-delete str i)
  (string-append (substring str 0 i)
                 (substring str (add1 i))))

(string=? (string-delete "hello" 0) "ello")
(string=? (string-delete "hello" 3) "helo")
(string=? (string-delete "hello" 4) "hell")

;; Can `string-delete` deal with empty strings?
;;
;; Answer:
;; It cannot! (Unless we added a special case to the function to e.g. return "")
