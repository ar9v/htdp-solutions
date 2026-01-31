#lang htdp/isl+

;;; Refine the definition of Xexpr so that you can represent XML elements, including items
;;; in enumerations, that are plain strings.

; An Xexpr is one of:
; -- (cons Symbol Body)
; -- (cons Symbol (cons [List-of Attribute] Body))
;
; Where Body is [List-of [Xexpr | XWord]]
; Where XWord is '(word ((text String)))
; Where Attribute is a two element list:
;   (cons Symbol (cons String '()))

(define example
  '(ul (li (word ((text "Item 1"))))
       (li (word ((text "Item 2"))))))
; <ul>
;   <li>Item 1</li>
;   <li>Item 2</li>
; </ul>
