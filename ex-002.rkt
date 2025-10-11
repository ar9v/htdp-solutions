#lang htdp/bsl

;; Add the following two lines to the definitions area:
(define prefix "hello")
(define suffix "world")

;; Then use string primitives to create an expression that concatenates `prefix`
;; and `suffix` and adds "_" between them.

(string=? (string-append prefix "_" suffix)
          "hello_world")
