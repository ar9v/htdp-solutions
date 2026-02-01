#lang htdp/isl+

(require 2htdp/abstraction)
(require 2htdp/image)

;;; The data definitions in figure 127 use `list`. Rewrite them so they use `cons`. Then
;;; use the recipe to design the rendering functions for XEnum.v2 and XItem.v2 from
;;; scratch. You should come up with the same definitions as in figure 128.

; an XWord is a list:
; (cons 'word (cons TextAttr '()))
;
; TextAttr is a list with a single 'text attr: (cons (cons 'text (cons String '())) '())

; An XItem is one of
; -- (cons 'li (cons XWord '())
; -- (cons 'li (cons Attribute* (cons XWord '())))
; -- (cons 'li (cons XEnum '()))
; -- (cons 'li (cons Attribute* (cons XEnum '())))

; An XEnum is one of
; -- (cons 'ul XItem*)
; -- (cons 'ul (cons Attribute* XItem*))

; XItem* is one of
; -- '()
; -- (cons XItem XItem*)

; Attribute* is one of
; -- '()
; -- (cons Attribute Attribute*)
;
; Attribute is (cons Symbol (cons String '()))

; XWords
(define hello '(word ((text "Hello"))))
(define world '(word ((text "World"))))
(define goodbye '(word ((text "Goodbye"))))

; Xexprs
(define a0 '((initial "X")))
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; XItems
(define hello-item `(li ,hello))
(define goodbye-item `(li ,goodbye))
(define nested-item `(li (ul
                          (li (ul ,goodbye-item))
                          ,hello-item)))

; XEnums
(define xenum-hello-goodbye `(ul ,hello-item ,goodbye-item))
(define xenum-nested `(ul ,hello-item ,nested-item))

(define SIZE 12) ; font size
(define COLOR "black") ; font color
(define BT ; a graphical constant
  (beside (circle 1 'solid 'black) (text " " SIZE COLOR)))

; bulletize: Image -> Image
; marks item with bullet
(check-expect (bulletize (square 3 "solid" "black"))
              (beside/align 'center BT (square 3 "solid" "black")))
(define (bulletize item)
  (beside/align 'center BT item))

; render-enum: XEnum -> Image
; renders an XEnum.v2 as an image
(check-expect (render-enum xenum-hello-goodbye)
              (above/align 'left
                           (render-item hello-item)
                           (above/align 'left
                                        (render-item goodbye-item) empty-image)))
(check-expect (render-enum xenum-nested)
              (above/align 'left
                           (render-item hello-item)
                           (above/align 'left
                                        (render-item nested-item)
                                        empty-image)))
(define (render-enum xenum)
  (local [(define (render-items is)
            (cond [(empty? is) empty-image]
                  [(cons? is)
                   (above/align 'left
                                (render-item (first is))
                                (render-items (rest is)))]))]
    (render-items (xexpr-content xenum))))

; render-item: XItem -> Image
; renders one XItem.v2 as an image
(check-expect (render-item hello-item) (bulletize (text "Hello" SIZE 'black)))
(check-expect (render-item nested-item)
              (bulletize (render-enum (first (xexpr-content nested-item)))))
(define (render-item xitem)
  (local [(define content (first (xexpr-content xitem)))]
    (bulletize
     (if (word? content)
         (text (word-text content) SIZE 'black)
         (render-enum content)))))

; Xexpr Helpers

; list-of-attributes? [[List-of Attribute] | Xexpr] -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (or (empty? x) (cons? (first x))))

; xexpr-name: Xexpr -> Symbol
; Produces the given element's tag
(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name (first (rest e2))) 'action)
(define (xexpr-name xe) (first xe))

; xexpr-content: Xexpr -> Body
; Produces the body of `xe`, meaning, the nested Xexprs within it
(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3)  '((action)))
(check-expect (xexpr-content e4) '((action) (action)))
(define (xexpr-content xe)
  (match xe
    [(list (? symbol?)) '()]
    [(list (? symbol?) (? list-of-attributes?)) '()]
    [(cons (? symbol?) (cons (? list-of-attributes?) (cons x xs))) (cons x xs)]
    [(cons (? symbol?) (cons x xs)) (cons x xs)]
    [other (error "Error: " other " is not a valid XML representation")]))


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
