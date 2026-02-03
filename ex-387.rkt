#lang htdp/isl+

;;; Design `cross`. The function consumes a list of symbols and a list of numbers and
;;; produces all possible ordered pairs of symbols and numbers. That is, when given
;;; '(a b c) and '(1 2), the expected result is '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2))

; cross: [List-of Symbol] [List-of Number] -> [List-of (list Symbol Number)]
; Computes the cartesian product of `ss` x `ns`
(check-expect (cross '() '(1 2)) '())
(check-expect (cross '(a b c) '()) '())
(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(define (cross ss ns)
  (cond [(empty? ss) '()]
        [else (append (map (λ (n) (list (first ss) n)) ns) (cross (rest ss) ns))]))
