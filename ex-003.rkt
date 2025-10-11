#lang htdp/bsl

;; Add the following two lines to the definitions area:
(define str "helloworld")
(define i 5)

;; Then create an expression using string primitives that adds "_" at the position `i`.
(string=?
 (string-append (substring str 0 i)
                "_"
                (substring str i))
 "hello_world")


