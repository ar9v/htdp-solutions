#lang htdp/isl+

;;; Develop a checked version of `bundle` that is guaranteed to terminate for all inputs.
;;; It may signal an error for those cases where the original version loops.


; bundle: [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
(check-expect (bundle (explode "abcdefg") 3) '("abc" "def" "g"))
(check-expect (bundle '("a" "b") 3) '("ab"))
(check-expect (bundle '() 3) '())
(check-error (bundle '("a" "b") 0))
(define (bundle s n) (map implode (list->chunks s n)))

; list->chunks: [List-of X] N -> [List-of [List-of X]]
; Chunks up `l` into a list of lists of size `n`
(check-error (list->chunks '() 0))
(check-expect (list->chunks '() 1) '())
(check-error (list->chunks '(a b c) 0))
(check-expect (list->chunks '(a b c d) 2) '((a b) (c d)))
(check-expect (list->chunks '(1 2 "a" "b" c d) 3) '((1 2 "a") ("b" c d)))
(define (list->chunks l n)
  (local [(define (chunk l n)
            (cond [(empty? l) '()]
                  [else (cons (take l n) (chunk (drop l n) n))]))]
    (if (zero? n)
        (error "Cannot chunk a list into groups of 0 elements!")
        (chunk l n))))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond [(or (zero? n) (empty? l)) '()]
        [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond [(or (zero? n) (empty? l)) l]
        [else (drop (rest l) (sub1 n))]))
