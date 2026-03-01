#lang htdp/isl+

;;; Design `to10`. It consumes a list of digits and produces the corresponding number.
;;; The first item on the list is the *most significant* digit. Hence, when applied to
;;; '(1 0 2), it produces 102.

;;; Domain Knowledge You may recall from grade school that the result is determined by
;;;
;;; 1 * 10^2 + 0 * 10^1 + 2 * 10^0 = ((1 * 10 + 0) * 10) + 2
;;;
;;; NOTE:
;;; or, a bit more obviously:
;;;   ((((0 * 10 + 1) * 10) + 0) * 10 + 2))

; Digit is an N in '(0 1 2 3 4 5 6 7 8 9)

; to10: [List-of Digit] -> N
; Turns the list of digits `l` into a number
(check-expect (to10 '()) 0)
(check-expect (to10 '(1 0 2)) 102)
(define (to10 l)
  (local [; to10/a: [List-of Digit] N -> N
          ; accumulator res: represents the base 10 number formed by the digits in `l`
          ; that are not in `sl`
          (define (to10/a sl res)
            (cond [(empty? sl) res]
                  [else (to10/a (rest sl) (+ (first sl) (* 10 res)))]))]
    (to10/a l 0)))
