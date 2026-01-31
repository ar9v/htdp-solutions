#lang htdp/isl+

(require 2htdp/abstraction)
(require 2htdp/image)

;;; Before you read on, equip the definition of `render-item1` with tests. Make sure to
;;; formulate these tests in such a way that they don't depend on the BT constant. Then
;;; explain *how* the function works; keep in mind that the purpose statement explains
;;; *what* it does.

(define FONT-SIZE 20)
(define BT (circle (/ FONT-SIZE 6) "solid" "black"))

(define item1 '(item (word ((text "Neal")))))
(define item2 '(item (word ((text "Jack")))))
(define item3 '(item (word ((text "Me")))))

; render-item1: XItem -> Image
; Renders an image of an XItem `i` by:
; -- plucking the content from `i` using `xexpr-content`
; -- since we're dealing with XItems, it grabs the `first` of the content, guaranteed
;    to be the word in the item
; -- extracts the string in the XML representation of the text, by using `word-text`
; -- creates a Text Image using the string and 2htdp/image's `text` function
; -- finally, it renders a bullet image, BT, and the text image, one after the other
(check-satisfied (render-item1 item1) (rendered-with-item-text? item1))
(check-satisfied (render-item1 item2) (rendered-with-item-text? item2))
(check-satisfied (render-item1 item3) (rendered-with-item-text? item3))
(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word FONT-SIZE 'black)))
    (beside/align 'center BT item)))

; xexpr-content: Xexpr -> Body
; Produces the body of `xe`, meaning, the nested Xexprs within it
(define (xexpr-content xe)
  (match xe
    [(list (? symbol?)) '()]
    [(list (? symbol?) (? list-of-attributes?)) '()]
    [(cons (? symbol?) (cons (? list-of-attributes?) (cons x xs))) (cons x xs)]
    [(cons (? symbol?) (cons x xs)) (cons x xs)]
    [other (error "Error: " other " is not a valid XML representation")]))

; word-text: XWord -> String
; extracts the value of `xword`
(define (word-text xword)
  (match xword [(list 'word (list (list 'text s))) s]))

; list-of-attributes? [[List-of Attribute] | Xexpr] -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (or (empty? x) (cons? (first x))))

; rendered-with-item-text?: XItem -> [Image -> Boolean]
; Checks if the rendered image has at least the width and height of an `text` image
; built with the provided `item`'s string.
;
; (The best I could come up with to avoid BT; but it assumes BT will e.g. share FONT-SIZE
;  which is itself a constant so... The idea is that the result from `render-item1`
;  must have at least the width and height of just rendering the text image, and this
;  text image should be built with the item's word's text)
;
; (We could also extract rendering BT into a function an express tests in terms of that,
;  but I figured the exercise wanted us to keep the function intact)
(define (rendered-with-item-text? item)
  (lambda (img)
    (local [(define text-img
              (text (word-text (first (xexpr-content item))) FONT-SIZE "black"))]
      (and (>= (image-width img) (image-width text-img))
           (>= (image-height img) (image-height text-img))))))
