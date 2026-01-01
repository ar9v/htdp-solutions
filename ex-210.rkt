#lang htdp/bsl+

;;; Complete the design of the `words->strings` function specified in figure 78.
;;;
;;; HINT: Use your solution to exercise 209.

; words->strings: List-of-words -> List-of-strings
; Returns of list of strings, the String representation of each Word in `words`
(check-expect (words->strings '()) '())
(check-expect (words->strings (list (list "a" "c" "t")
                                    (list "c" "a" "t")
                                    (list "b" "u" "n")))
              (list "act" "cat" "bun"))
(define (words->strings words)
  (cond [(empty? words) '()]
        [(cons? words)
         (cons (word->string (first words))
               (words->strings (rest words)))]))

; word->string: Word -> String
; converts `w` into a String
(check-expect (word->string '()) "")
(check-expect (word->string (list "a" "c" "t")) "act")
(define (word->string w)
  (implode w))
