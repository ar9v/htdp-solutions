#lang htdp/isl+

(require 2htdp/abstraction)

;;; Make up three examples for XWords. Design `word?`, which checks whether some ISL+ value
;;; is an XWord, and `word-text`, which extracts the value of the only attribute of an
;;; instance of XWord

; An XWord is '(word ((text String)))
(define hello '(word ((text "Hello"))))
(define world '(word ((text "World"))))
(define goodbye '(word ((text "Goodbye"))))

; word?: Any -> Boolean
; Checks whether `e` is an XWord
(check-expect (word? hello) #true)
(check-expect (word? world) #true)
(check-expect (word? goodbye) #true)
(check-expect (word? 1) #false)
(check-expect (word? '(machine ((initial "red")) (action ((state "red") (next "green")))))
              #false)
(define (word? e)
  (match e
    [(list 'word (list (list 'text (? string?)))) #true]
    [_else #false]))

; word-text: XWord -> String
; extracts the value of `xword`
(check-expect (word-text hello) "Hello")
(check-expect (word-text world) "World")
(check-expect (word-text goodbye) "Goodbye")
(define (word-text xword)
  (match xword [(list 'word (list (list 'text s))) s]))
