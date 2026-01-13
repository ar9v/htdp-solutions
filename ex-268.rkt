#lang htdp/isl

;;; An inventory record specifies the name of an item, a description, the acquisition price
;;; and the recommended sales price.
;;;
;;; Define a function that sorts a list of inventory records by the difference of two
;;; prices.

(define-struct ir [name description price sales-price])

; sort-by-price: [List-of IR] [IR IR -> Boolean] -> [List-of IR]
; Sorts the inventory records by their `price`, using `cmp` to compare between the two
; `ir-price`s
(check-expect (sort-by-price '() <) '())
(check-expect (sort-by-price '() >) '())
(check-expect (sort-by-price (list (make-ir "Item 1" "Description 1" 4 6)
                                   (make-ir "Item 2" "Description 2" 2 3)
                                   (make-ir "Item 3" "Description 3" 7 8)
                                   (make-ir "Item 4" "Description 4" 1 3)
                                   (make-ir "Item 5" "Description 5" 3 6))
                             <)
              (list (make-ir "Item 4" "Description 4" 1 3)
                    (make-ir "Item 2" "Description 2" 2 3)
                    (make-ir "Item 5" "Description 5" 3 6)
                    (make-ir "Item 1" "Description 1" 4 6)
                    (make-ir "Item 3" "Description 3" 7 8)))
(check-expect (sort-by-price (list (make-ir "Item 1" "Description 1" 4 6)
                                   (make-ir "Item 2" "Description 2" 2 3)
                                   (make-ir "Item 3" "Description 3" 7 8)
                                   (make-ir "Item 4" "Description 4" 1 3)
                                   (make-ir "Item 5" "Description 5" 3 6))
                             >)
              (list (make-ir "Item 3" "Description 3" 7 8)
                    (make-ir "Item 1" "Description 1" 4 6)
                    (make-ir "Item 5" "Description 5" 3 6)
                    (make-ir "Item 2" "Description 2" 2 3)
                    (make-ir "Item 4" "Description 4" 1 3)))
(define (sort-by-price irs cmp)
  (local ((define (cmp-irs ir1 ir2) (cmp (ir-price ir1) (ir-price ir2))))
    (sort irs cmp-irs)))
