#lang htdp/bsl

;;; Design `convertFC`. The function converts a list of measurements in Fahrenheit to a
;;; list of Celsius measurements.

; convertFC: List-of-F -> List-of-C
; Converts temperatures in `lof` into a list of Celsius temps
(check-expect (convertFC '()) '())
(check-expect (convertFC (cons 212 (cons 32 (cons 44 '()))))
              (cons (fahrenheit->celsius 212)
                    (cons (fahrenheit->celsius 32)
                          (cons (fahrenheit->celsius 44)
                                '()))))
(define (convertFC lof)
  (cond [(empty? lof) '()]
        [(cons? lof)
         (cons (fahrenheit->celsius (first lof))
               (convertFC (rest lof)))]))

; fahrenheit->celsius: Fahrenheit -> Celsius
; Converts `f` into a Celsius temp
(check-expect (fahrenheit->celsius 212) 100)
(check-expect (fahrenheit->celsius 32) 0)
(check-expect (fahrenheit->celsius 44) (* 5/9 (- 44 32)))
(define (fahrenheit->celsius f)
  (* 5/9 (- f 32)))
