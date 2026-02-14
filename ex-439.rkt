#lang htdp/isl+

;;; Copy `gcd-structural` into DrRacket and evaluate
;;; (time (gcd-structural 101135853 45014640))

(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))

; cpu time: 1378 real time: 1380 gc time: 0
(time (gcd-structural 101135853 45014640))
