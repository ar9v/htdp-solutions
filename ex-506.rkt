#lang htdp/isl+

;;; Design an accumulator-style version of `map`

; map/a: [X -> Y] [List-of X] -> [List-of Y]
; Constructs a list by gathering the results of applying `f` to each element in `l`
(check-expect (map.v2 (λ (x) (expt x 2)) '()) '())
(check-expect (map.v2 (λ (x) (expt x 2)) '(1 2 3)) '(1 4 9))
(check-expect (map.v2 explode '("hey" "there")) '(("h" "e" "y") ("t" "h" "e" "r" "e")))
(define (map.v2 f l)
  (local [(define (map/a sl res)
            (cond [(empty? sl) (reverse res)]
                  [else (map/a (rest sl) (cons (f (first sl)) res))]))]
    (map/a l '())))
