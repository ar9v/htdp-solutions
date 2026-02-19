#lang htdp/isl+

;;; Design the function `tokenize`. It turns a Line into a list of tokens. Here a token
;;; is either a 1String or a String that consists of lowercase letters and nothing else.
;;; That is, all white-space 1Strings are dropped; all other non-letters remain as is; and
;;; all consecutive letters are bundled into "words".
;;;
;;; HINT: Read up on the `string-whitespace?` function.

; a Token is a String
; constraint: It is exclusively made up of lowercase letters.

; tokenize: Line -> [List-of Token]
; Turns a line into a list of tokens
(check-expect (tokenize '()) '())
(check-expect (tokenize '("a" "b" "c")) '("abc"))
(check-expect (tokenize '("a" " " "b" "c")) '("a" "bc"))
(check-expect (tokenize '("a" "!" "B" "c")) '("a" "!" "B" "c"))
(define (tokenize l)
  (local [(define (first-token l) (implode (take-while string-lower-case? l)))
          (define (remove-first-token l) (drop-while string-lower-case? l))]
    (cond [(empty? l) '()]
          [(string-whitespace? (first l)) (tokenize (rest l))]
          [(not (string-lower-case? (first l))) (cons (first l) (tokenize (rest l)))]
          [else (cons (first-token l) (tokenize (remove-first-token l)))])))

; take-while: [Any -> Boolean] [List-of Any] -> [List-of Any]
; Produces a list of the elements in `l` that fulfill `pred`, up til the element that
; does not fulfill `pred`
(check-expect (take-while even? '()) '())
(check-expect (take-while even? '(1 2 3)) '())
(check-expect (take-while even? '(2 4 6 3 8)) '(2 4 6))
(define (take-while pred l)
  (cond [(or (empty? l) (not (pred (first l)))) '()]
        [else (cons (first l) (take-while pred (rest l)))]))

; drop-while: [Any -> Boolean] [List-of Any] -> [List-of Any]
; Returns a list where the first elements that fulfill `pred` are removed, up til the
; first element that does not fulfill `pred`.
(check-expect (drop-while even? '()) '())
(check-expect (drop-while even? '(2 4 6)) '())
(check-expect (drop-while even? '(2 4 6 3 8)) '(3 8))
(define (drop-while pred l)
  (cond [(empty? l) '()]
        [(pred (first l)) (drop-while pred (rest l))]
        [else l]))
