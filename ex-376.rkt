#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design a program that counts all "hello"'s in an instance of XEnum.v2

; Xenums
(define one-hello '(ul (li (word ((text "hello"))))))
(define three-hello-flat
  '(ul (li (word ((text "hello"))))
       (li (word ((text "hello"))))
       (li (word ((text "hello"))))))
(define three-hello-nested
  '(ul (li (word ((text "hello"))))
       (ul (li (word ((text "hello")))))
       (ul (li (ul (li (word ((text "hello")))))))))
(define no-hellos
  '(ul (li (word ((text "goodbye"))))
       (li (word ((text "goodbye"))))
       (ul (li (ul (li (word ((text "goodbye")))))))))

; count-words: XEnum String -> Number
; Counts how many times the text `str` appears in `xenum`
(check-expect (count-words no-hellos "hello") 0)
(check-expect (count-words one-hello "hello") 1)
(check-expect (count-words three-hello-flat "hello") 3)
(check-expect (count-words three-hello-nested "hello") 3)
(check-expect (count-words no-hellos "goodbye") 3)
(define (count-words xenum str)
  (local [(define (count-word/item item)
            (local [(define first-child (first (xexpr-content item)))]
              (cond [(word? first-child)
                     (if (string=? (word-text first-child) str) 1 0)]
                    [else (count-words item str)])))
          (define (deal-with-one item so-far) (+ (count-word/item item) so-far))]
    (foldr deal-with-one 0 (xexpr-content xenum))))

; hellos: XEnum -> Number
; Checks how many times the text "hello" appears in `xenum`
(check-expect (count-hello no-hellos) 0)
(check-expect (count-hello one-hello) 1)
(check-expect (count-hello three-hello-flat) 3)
(check-expect (count-hello three-hello-nested) 3)
(define (count-hello xenum)
  (count-words xenum "hello"))

; xexpr-content: Xexpr -> Body
; Produces the body of `xe`, meaning, the nested Xexprs within it
(define (xexpr-content xe)
  (match xe
    [(list (? symbol?)) '()]
    [(list (? symbol?) (? list-of-attributes?)) '()]
    [(cons (? symbol?) (cons (? list-of-attributes?) (cons x xs))) (cons x xs)]
    [(cons (? symbol?) (cons x xs)) (cons x xs)]
    [other (error "Error: " other " is not a valid XML representation")]))

; list-of-attributes? [[List-of Attribute] | Xexpr] -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (or (empty? x) (cons? (first x))))
; word?: Any -> Boolean
; Checks whether `e` is an XWord
(define (word? e)
  (match e
    [(list 'word (list (list 'text (? string?)))) #true]
    [_else #false]))

; word-text: XWord -> String
; extracts the value of `xword`
(define (word-text xword)
  (match xword [(list 'word (list (list 'text s))) s]))
