#lang htdp/isl+

;;; Design the function `is-prime`, which consumes a natural number and returns #true if
;;; it is prime and #false otherwise.
;;;
;;; DOMAIN KNOWLEDGE:
;;; A number `n` is prime if it not divisible by any number between `n - 1` and 2

; is-prime?: N[>= 1] -> Boolean
; Determines if given number `n` is prime
(check-expect (is-prime? 2) #true)
(check-expect (is-prime? 4) #false)
(check-expect (is-prime? 561) #false)
(check-expect (is-prime? 71) #true)
(define (is-prime? n)
  (local [; prime?/a: N[>=1] -> Boolean
          ; Determines whether `np` can divide `n`
          ;
          ; accumulator np: represents a number greater than 2 and
          ; lower than n
          ;
          ; generative: if no number between 2 and sqrt(n) can
          ; divide n, then n must be prime
          (define (prime?/a np)
            (or (< n (expt np 2))
                (and (not (= (remainder n np) 0))
                     (prime?/a (add1 np)))))]
    (prime?/a 2)))
