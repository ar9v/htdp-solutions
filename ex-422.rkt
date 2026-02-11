#lang htdp/isl+

;;; Define the function `list->chunks`. It consumes a list `l` of arbitrary data and
;;; a natural number `n`. The function's result is a list of list chunks of size `n`. Each
;;; chunk represents a sub-sequence of items in `l`. Use `list->chunks` to define `bundle`
;;; via function composition.

; list->chunks: [List-of X] N -> [List-of [List-of X]]
; Chunks up `l` into a list of lists of size `n`
(check-expect (list->chunks '() 0) '())
(check-expect (list->chunks '() 1) '())
(check-expect (list->chunks '(a b c) 0) '())
(check-expect (list->chunks '(a b c d) 2) '((a b) (c d)))
(check-expect (list->chunks '(1 2 "a" "b" c d) 3) '((1 2 "a") ("b" c d)))
(define (list->chunks l n)
  (local [(define (chunk l n)
            (cond [(empty? l) '()]
                  [else (cons (take l n) (chunk (drop l n) n))]))]
    (if (zero? n) '() (chunk l n))))

; bundle: [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
(check-expect (bundle (explode "abcdefg") 3) '("abc" "def" "g"))
(check-expect (bundle '("a" "b") 3) '("ab"))
(check-expect (bundle '() 3) '())
(check-expect (bundle '("a" "b") 0) '())
(define (bundle s n)
  (map implode (list->chunks s n)))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))
