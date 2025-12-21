#lang htdp/bsl+

;;; Design the `prefixes` function, which consumes a list of 1Strings and produces the
;;; list of all prefixes. A list `p` is a *prefix* of `l` if `p` and `l` are the same
;;; up through all items in `p`.
;;;
;;; For example: `(list "a" "b" "c")` is a prefix of itself and `(list "a" "b" "c" "d")`.

; prefixes: List<1String> -> List<List<1String>>
; Computes all of `l`'s prefixes
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a")) (list (list "a")))
(check-expect (prefixes (list "a" "b"))
              (list (list "a" "b") (list "a")))
(check-expect (prefixes (list "a" "b" "c"))
              (list (list "a" "b" "c") (list "a" "b") (list "a")))
(define (prefixes l)
  (cond [(empty? l) '()]
        [(cons? l) (append (list l) (prefixes (but-last l)))]))

; but-last: List<1String> -> List<1String>
; Returns a list made up of `l`'s elements, save for the last one
(check-expect (but-last '()) '())
(check-expect (but-last (list 1)) '())
(check-expect (but-last (list 1 2)) (list 1))
(define (but-last l)
  (cond [(or (empty? l) (empty? (rest l))) '()]
        [else (cons (first l) (but-last (rest l)))]))


;;; Design the function `suffixes`, which consumes a list of 1Strings and produces all
;;; `suffixes`. A list `s` is a *suffix* of `l` if `s` and `l` are the same from the end,
;;; up through all items in `s`.
;;;
;;; For example: `(list "b" "c" "d")` is a suffix of itself and `(list "a" "b" "c" "d")`

; suffixes: List<1String> -> List<List<1String>>
; Computes all of `l`'s suffixes
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a")) (list (list "a")))
(check-expect (suffixes (list "a" "b" "c"))
              (list (list "a" "b" "c") (list "b" "c") (list "c")))
(define (suffixes l)
  (cond [(empty? l) '()]
        [(cons? l) (cons l (suffixes (rest l)))]))
