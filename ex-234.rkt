#lang htdp/bsl+

(require 2htdp/web-io)

;;; Create the function `make-ranking`, which consumes a list of ranked song titles and
;;; produces a list representation of an HTML table. Consider this example:

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

;;; If you apply `make-ranking` to `one-list` and display the resulting web page in a
;;; browser, you should see something like Figure 85.

; An Entry is a list with two elements:
;   (list Number String)
;
; interpretation: (list n s) is represents a song `s`, ranked in position `n`

; ranking: [String] -> ...
(check-expect (ranking one-list)
              (list (list 1 "Asia: Heat of the Moment")
                    (list 2 "U2: One")
                    (list 3 "The White Stripes: Seven Nation Army")))
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; add-ranks: [String] -> [Entry]
; Turn `los` into ranking entries.
(check-expect (add-ranks one-list)
              (list (list 3 "Asia: Heat of the Moment")
                    (list 2 "U2: One")
                    (list 1 "The White Stripes: Seven Nation Army")))
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

; make-ranking: [Entry] -> HTML
; Turns a list of entries into an HTML table
(check-expect (make-ranking one-list)
              `(html
                (head (title "Rankings"))
                (body
                 (table ((border "1")) ,@(make-table (ranking one-list))))))
(define (make-ranking entries)
  `(html
    (head (title "Rankings"))
    (body
     (table ((border "1")) ,@(make-table (ranking entries))))))

; make-table: [Entry] -> [Tr]
; Turns entries into their `<tr>` representations
(check-expect (make-table (ranking one-list))
              '((tr (td "1") (td "Asia: Heat of the Moment"))
                (tr (td "2") (td "U2: One"))
                (tr (td "3") (td "The White Stripes: Seven Nation Army"))))
(define (make-table entries)
  (cond [(empty? entries) '()]
        [(cons? entries)
         (cons `(tr ,@(make-row (first entries)))
               (make-table (rest entries)))]))

; make-row: [String | Number] -> nested list
; creates a row for an HTML table from l
(check-expect (make-row (list 1 "Asia: Heat of the Moment"))
              '((td "1") (td "Asia: Heat of the Moment")))
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))

; String | Number -> nested list
; creates a cell for an HTML table from a number
(check-expect (make-cell 1) '(td "1"))
(check-expect (make-cell "foo") '(td "foo"))
(define (make-cell n-or-s)
  `(td ,(if (number? n-or-s) (number->string n-or-s) n-or-s)))

(show-in-browser (make-ranking one-list))
