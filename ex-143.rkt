#lang htdp/bsl

;;; Determine how `average` behaves in DrRacket when applied to the empty list. Then
;;; design `checked-average`, a function that produces an informative error message when
;;; it is applied to '()

;;; A:
;;; When applied to the empty list, BSL will complain that we're dividing by 0, since
;;; `how-many` will yield 0

; List-of-temperatures -> Number
; computes the average temperature
(define (average alot)
  (/ (sum alot) (how-many alot)))

; List-of-temperatures -> Number
; adds up the temperatures on the given list
(define (sum alot)
  (if (empty? alot)
      0
      (+ (first alot) (sum (rest alot)))))

; List-of-temperatures -> Number
; counts the temperatures on the given list
(define (how-many alot)
  (if (empty? alot)
      0
      (+ 1 (how-many (rest alot)))))

; checked-average: List-of-temperatures -> NumberOrError
; Averages `alot` iff `alot` is not empty; produces an error otherwise.
(check-error (checked-average '()))
(check-expect (checked-average (cons 1 (cons 2 (cons 3 '())))) 2)
(define (checked-average alot)
  (cond [(empty? alot)
         (error "Cannot average an empty list of numbers!")]
        [else (average alot)]))
