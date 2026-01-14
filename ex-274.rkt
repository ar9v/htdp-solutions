#lang htdp/isl

;;; Use existing abstractions to define the `prefixes` and `suffixes` functions from
;;; exercise 190. Ensure that they pass the same tests as the original function.

; take: Number [List-of X] -> [List-of X]
; Return the first `n` elements of `l`
(check-expect (take 3 '()) '())
(check-expect (take 3 '(1 2 3 4 5 6 7)) '(1 2 3))
(define (take n l)
  (cond [(or (zero? n) (empty? l)) '()]
        [else (cons (first l) (take (sub1 n) (rest l)))]))

; drop: Number [List-of X]
; Takes the last (- (length l) n) elements of `l`
(check-expect (drop 3 '()) '())
(check-expect (drop 3 '(1 2 3 4 5 6 7)) '(4 5 6 7))
(define (drop n l)
  (cond [(or (zero? n) (empty? l)) l]
        [else (drop (sub1 n) (rest l))]))

; prefixes: [List-of 1String] -> [List-of [List-of 1String]]
; Computes all of `l`'s prefixes
;
; Note: In the original, the order was reversed; I'm not bothering with that. The essential
; thing is that we get the same elements.
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a")) (list (list "a")))
(check-expect (prefixes (list "a" "b"))
              (list (list "a") (list "a" "b")))
(check-expect (prefixes (list "a" "b" "c"))
              (list (list "a") (list "a" "b") (list "a" "b" "c")))
(define (prefixes l)
  (local [(define (take-n-+1-from-l n) (take (add1 n) l))]
    (build-list (length l) take-n-+1-from-l)))

; suffixes: [List-of 1String] -> [List-of [List-of 1String]]
; Computes all of `l`'s suffixes
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a")) (list (list "a")))
(check-expect (suffixes (list "a" "b" "c"))
              (list (list "a" "b" "c") (list "b" "c") (list "c")))
(define (suffixes l)
  (local [(define (drop-n-from-l n) (drop n l))]
    (build-list (length l) drop-n-from-l)))
