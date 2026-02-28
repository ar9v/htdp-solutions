#lang htdp/isl+

;;; Design the function `palindrome`, which accepts a non-empty list and constructs a
;;; palindrome by mirroring the list around the last item. When given `(explode "abc")`,
;;; it yields `(explode "abcba")`

; palindrome: [Non-Empty [List-of X]] -> [Non-Empty [List-of X]]
; Mirrors the given non-empty list `nel`, using the last item as a pivot.
(check-expect (palindrome '("u")) '("u"))
(check-expect (palindrome (explode "abc")) (explode "abcba"))
(check-expect (palindrome '(4 2 3)) '(4 2 3 2 4))
(define (palindrome nel)
  (local [; accumulator rev: represents the reversed list of items in `nel` that are not
          ; in `nel-1`.
          (define (palindrome/a nel-1 rev)
            (cond [(empty? (rest nel-1)) (append nel rev)]
                  [else (palindrome/a (rest nel-1) (cons (first nel-1) rev))]))]
    (palindrome/a nel '())))
