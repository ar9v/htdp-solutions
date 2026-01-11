#lang htdp/isl

;;; Use the `contains?` function to define functions that search for "atom", "basic", and
;;; "zoo", respectively.

; contains?: String List<String> -> Boolean
; determines whether `l` contains the string `s`
(define (contains? s l)
  (cond [(empty? l) #false]
        [else (or (string=? s (first l))
                  (contains? s (rest l)))]))


; contains-atom?: List<String> -> Boolean
; determines whether the string "atom" appears in `los`
(define (contains-atom? los)
  (contains? "atom" los))

; contains-basic?: List<String> -> Boolean
; determines whether the string "basic" appears in `los`
(define (contains-basic? los)
  (contains? "basic" los))

; contains-zoo?: List<String> -> Boolean
; determines whether the string "zoo" appears in `los`
(define (contains-zoo? los)
  (contains? "zoo" los))
