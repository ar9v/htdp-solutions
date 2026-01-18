#lang htdp/isl+

;;; Define `eliminate-exp`. The function consumes a number, `ua`, and a list of
;;; inventory records, and it produces a list of all those structures whose sales price is
;;; below `ua`.

(define-struct ir [name description price sales-price])

(define example-irs
  (list (make-ir "Milk" "Brand 1" 4 5)
        (make-ir "Bread" "Brand 1" 2 3)
        (make-ir "Milk" "Brand 2" 7 8)
        (make-ir "Egg" "Brand 3" 1 3)
        (make-ir "Orange Juice" "Brand 4" 3 6)))

; eliminate-exp: Number [List-of IR] -> [List-of IR]
; Picks IRs from `irs` which have an acquisition price lower than `ua`
(check-expect (eliminate-expensive 3 '()) '())
(check-expect (eliminate-expensive 6 example-irs)
              (list (make-ir "Milk" "Brand 1" 4 5)
                    (make-ir "Bread" "Brand 1" 2 3)
                    (make-ir "Egg" "Brand 3" 1 3)
                    (make-ir "Orange Juice" "Brand 4" 3 6)))
(define (eliminate-expensive ua irs)
  (filter (λ (ir) (< (ir-price ir) ua)) irs))

;;; Then use `filter` to define `recall`, which consumes the name of an inventory item,
;;; called `ty`, and a list of inventory records and produces a list of inventory records
;;; that do not use the name `ty`.

; recall: String [List-of IR] -> [List-of IR]
; Picks all items from `irs` whose name is not `ty`
(check-expect (recall "foo" '()) '())
(check-expect (recall "Milk" example-irs)
              (list (make-ir "Bread" "Brand 1" 2 3)
                    (make-ir "Egg" "Brand 3" 1 3)
                    (make-ir "Orange Juice" "Brand 4" 3 6)))
(define (recall ty irs)
  (filter (λ (ir) (not (string=? (ir-name ir) ty))) irs))

;;; In addition, define `selection`, which consumes two lists of names and selects all
;;; those from the second one that are also on the first.

; selection: [List-of String] [List-of String] -> [List-of String]
; Returns a list of names in common between `los1` and `los2`
(check-expect (selection '() '()) '())
(check-expect (selection '("foo" "bar") '("baz")) '())
(check-expect (selection '("foo" "bar") '("baz" "quux" "bar")) '("bar"))
(define (selection los1 los2)
  (filter (λ (s) (member? s los1)) los2))
