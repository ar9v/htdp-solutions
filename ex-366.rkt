#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `xexpr-name` and `xexpr-content`

(define a0 '((initial "X")))

(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

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

; xexpr-content.v2: Xexpr -> Body
; Produces the body of `xe`, meaning, the nested Xexprs within it
(check-expect (xexpr-content.v2 e0) '())
(check-expect (xexpr-content.v2 e1) '())
(check-expect (xexpr-content.v2 e2) '((action)))
(check-expect (xexpr-content.v2 e3)  '((action)))
(check-expect (xexpr-content.v2 e4) '((action) (action)))
(define (xexpr-content.v2 xe)
  (local [(define attrs-or-content (rest xe))]
    (cond [(empty? attrs-or-content) '()]
          [else
           (if (list-of-attributes? (first attrs-or-content))
               (rest attrs-or-content)
               attrs-or-content)])))

; list-of-attributes? [[List-of Attribute] | Xexpr] -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (or (empty? x) (cons? (first x))))
