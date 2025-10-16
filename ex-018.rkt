#lang htdp/bsl

;; Define the function `string-join`, which consumes two strings and appends them with
;; "_" in between. See exercise 2 for ideas.
(define (string-join s1 s2)
  (string-append s1 "_" s2))

(string=? (string-join "hello" "world")
          "hello_world")
