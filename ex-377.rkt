#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design a program that replaces all "hello"'s with "bye" in an enumeration

; XItem
(define xitem1 '(li (word ((text "hello")))))
(define xitem2 '(li (word ((text "hey")))))
(define xitem3 '(li (ul (li (word ((text "hello")))))))

; Xenums
(define xenum1 `(ul ,xitem1))
(define xenum2 `(ul ,xitem2 ,xitem3))
(define xenum2-with-attrs `(ul ((display "flex")) ,xitem2 ,xitem3))

; replace-with: XEnum String String -> XEnum
; Produces a new XEnum with all instances of `old` replaced with `new`
(check-expect (replace-with xenum1 "goodbye" "hey") xenum1)
(check-expect (replace-with xenum1 "hello" "goodbye")
              '(ul (li (word ((text "goodbye"))))))
(check-expect (replace-with xenum2 "hello" "goodbye")
              '(ul (li (word ((text "hey"))))
                   (li (ul (li (word ((text "goodbye"))))))))
(check-expect (replace-with xenum2-with-attrs "hello" "goodbye")
              '(ul ((display "flex"))
                   (li (word ((text "hey"))))
                   (li (ul (li (word ((text "goodbye"))))))))
(define (replace-with xenum old new)
  (local [(define (deal-with-one item so-far) (cons (replace-with/item item old new) so-far))
          (define attrs (xexpr-attr xenum))
          (define new-items (foldr deal-with-one '() (xexpr-content xenum)))]
    (if (empty? attrs)
        `(ul ,@new-items)
        `(ul ,attrs ,@new-items))))

; replace-with/item: XItem String String
; Replaces the text in `item` with `new` if it matches `old`
(check-expect (replace-with/item xitem1 "goodbye" "hey") xitem1)
(check-expect (replace-with/item xitem1 "hello" "goodbye") '(li (word ((text "goodbye")))))
(check-expect (replace-with/item xitem3 "hello" "goodbye")
              '(li (ul (li (word ((text "goodbye")))))))
(define (replace-with/item xitem old new)
  (local [(define first-child (first (xexpr-content xitem)))]
    `(li
      ,(cond [(word? first-child)
              (if (string=? (word-text first-child) old)
                  `(word ((text ,new)))
                  first-child)]
             [else (replace-with first-child old new)]))))

; replace-hello-bye: XEnum -> XEnum
(define (replace-hello-bye xenum)
  (replace-with xenum "hello" "bye"))

; xexpr-content: Xexpr -> Body
; Produces the body of `xe`, meaning, the nested Xexprs within it
(define (xexpr-content xe)
  (match xe
    [(list (? symbol?)) '()]
    [(list (? symbol?) (? list-of-attributes?)) '()]
    [(cons (? symbol?) (cons (? list-of-attributes?) (cons x xs))) (cons x xs)]
    [(cons (? symbol?) (cons x xs)) (cons x xs)]
    [other (error "Error: " other " is not a valid XML representation")]))

; xexpr-attr: Xexpr -> [List-of Attribute]
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

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
