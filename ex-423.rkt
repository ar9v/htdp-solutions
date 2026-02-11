#lang htdp/isl+

;;; Define `partition`. It consumes a String `s` and a natural number `n`. The function
;;; produces a list of string chunks of size `n`.
;;;
;;; For non-empty strings `s` and positive natural numbers `n`,
;;;
;;; (equal? (partition s n) (bundle (explode s) n))
;;;
;;; is #true. But don't use this equality as the definition for `partition`; use
;;; `substring` instead.
;;;
;;; HINT: Have `partition` produce its natural result for the empty string. For the case
;;; where `n` is 0, see exercise 421.

; partition: String N -> [List-of String]
; Breaks `s` apart into a list of strings of size `n`
(define size-limit 500000)
(define random-s (implode (build-list (random size-limit) (λ (_n) "a"))))
(define random-n (random size-limit))
(check-satisfied (partition random-s random-n)
                 (λ (res) (equal? res (bundle (explode random-s) random-n))))
(define (partition s n)
  (local [(define i (min (string-length s) n))]
    (cond [(or (string=? s "") (zero? n)) '()]
          [else (cons (substring s 0 i) (partition (substring s i) n))])))

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
