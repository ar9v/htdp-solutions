#lang htdp/bsl

;;; Develop the `contains?` function, which determines whether some given string occurs
;;; on a given list of strings.
;;;
;;; (do NOT use `member?`)

; contains?: String List-of-strings -> Boolean
; determines whether `s` is in `los`
(check-expect (contains? "a" '()) #false)
(check-expect (contains? "a" (cons "a" '())) #true)
(check-expect (contains? "a" (cons "b" (cons "a" '()))) #true)
(check-expect (contains? "a" (cons "b" (cons "c" '()))) #false)
(define (contains? s los)
  (cond [(empty? los) #false]
        [else
         (or (string=? s (first los))
             (contains? s (rest los)))]))
