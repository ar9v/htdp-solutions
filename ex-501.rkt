#lang htdp/isl+

;;; Design an accumulator-style version of `add-to-pi`. The function adds a natural
;;; number to `pi` without using `+`:

; add-to-pi: N -> Number
; adds n to pi without using +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n)
  (cond [(zero? n) pi]
        [else (add1 (add-to-pi (sub1 n)))]))

; add-to-pi.v2: N -> Number
; adds n to pi without using +
(check-within (add-to-pi.v2 2) (+ 2 pi) 0.001)
(define (add-to-pi.v2 n)
  (local [; add-pi/a: N N -> N
          ; adds `np` to `pi` without using `+`
          ;
          ; accumulator a: represents the sum of `pi` with the  amount of elements in the
          ;                range [n, np)
          (define (add-pi/a np a)
            (cond [(zero? np) a]
                  [else (add-pi/a (sub1 np) (add1 a))]))]
    (add-pi/a n pi)))
