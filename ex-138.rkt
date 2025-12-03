#lang htdp/bsl

;;; Here is a data definition for representing sequences of amounts of money:

; A List-of-amounts is one of:
; -- '()
; -- (cons PositiveNumber List-of-amounts)

;;; Create some examples to make sure you understand the data definition. Also add an
;;; arrow for the self reference.

;;; Using the first clause
(define empty-example '())

;;; Using the second clause,
(define ex-1 (cons 1 '()))
;;                    ^-- List-of-amounts


;;; Using the second clause, repeatedly
(define ex-2
  (cons 1 ; -> PositiveNumber
        (cons 2
              (cons 3 '()))))
;       |     |        |-> List-of-amounts
;       |     -> List-of-amounts
;       --> List-of amounts


;;; Design the `sum` function, which consumes a List-of-amounts and computes the sum
;;; of the amounts.

; sum: List-of-amounts -> PositiveNumber
; Adds the amounts in `loa`
(check-expect (sum empty-example) 0)
(check-expect (sum ex-1) 1)
(check-expect (sum ex-2) 6)
(define (sum loa)
  (cond [(empty? loa) 0]
        [(cons? loa)
         (+ (first loa) (sum (rest loa)))]))


;;; Use DrRacket's stepper to see how (sum l) works for a short list l in List-of-amounts.

(sum (cons 1 (cons 2 '())))

;; (sum (cons 1 (cons 2 '())))
;; ==
;; (cond [(empty? (cons 1 (cons 2 '()))) 0]
;;       [(cons? (cons 1 (cons 2 '())))
;;        (+ (first (cons 1 (cons 2 '())))
;;           (sum (rest (cons 1 (cons 2 '())))))])
;; ==
;; (cond [#false 0]
;;       [(cons? (cons 1 (cons 2 '())))
;;        (+ (first (cons 1 (cons 2 '())))
;;           (sum (rest (cons 1 (cons 2 '())))))])
;; ==
;; (cond [(cons? (cons 1 (cons 2 '())))
;;        (+ (first (cons 1 (cons 2 '())))
;;           (sum (rest (cons 1 (cons 2 '())))))])
;; ==
;; (cond [#true
;;        (+ (first (cons 1 (cons 2 '())))
;;           (sum (rest (cons 1 (cons 2 '())))))])
;; ==
;; (+ (first (cons 1 (cons 2 '())))
;;    (sum (rest (cons 1 (cons 2 '())))))
;; ==
;; (+ 1
;;    (sum (rest (cons 1 (cons 2 '())))))
;; ==
;; (+ 1
;;    (sum (cons 2 '())))
;; ==
;; (+ 1
;;    (cond [(empty? (cons 2 '())) 0]
;;          [(cons? (cons 2 '()))
;;           (+ (first (cons 2 '()))
;;              (sum (rest (cons 2 '()))))]))
;; ==
;; (+ 1
;;    (cond [#false 0]
;;          [(cons? (cons 2 '()))
;;           (+ (first (cons 2 '()))
;;              (sum (rest (cons 2 '()))))]))
;; ==
;; (+ 1
;;    (cond [(cons? (cons 2 '()))
;;           (+ (first (cons 2 '()))
;;              (sum (rest (cons 2 '()))))]))
;; ==
;; (+ 1
;;    (cond [#true
;;           (+ (first (cons 2 '()))
;;              (sum (rest (cons 2 '()))))]))
;; ==
;; (+ 1
;;    (+ (first (cons 2 '()))
;;       (sum (rest (cons 2 '())))))
;; ==
;; (+ 1
;;    (+ 2
;;       (sum (rest (cons 2 '())))))
;; ==
;; (+ 1
;;    (+ 2
;;       (sum '())))
;; ==
;; (+ 1
;;    (+ 2
;;       (cond [(empty? '()) 0]
;;             [(cons? '())
;;              (+ (first '()) (rest '()))])))
;; ==
;; (+ 1
;;    (+ 2
;;       (cond [#true 0]
;;             [(cons? '())
;;              (+ (first '()) (rest '()))])))
;; ==
;; (+ 1
;;    (+ 2
;;       0))
;; ==
;; (+ 1
;;    2)
;; ==
;; 3
