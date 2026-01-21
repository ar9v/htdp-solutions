#lang htdp/isl+

(require 2htdp/abstraction)

;;; Use loops to define a function that
;;;
;;; 1. creates the list (list 0 ... (- n 1)) for any natural number n;
;;; 2. creates the list (list 1 ... n) for any natural number n;
;;; 3. creates the list (list 1 1/2 ... 1/n) for any natural number n;
;;; 4. creates the list of the first n even numbers; and
;;; 5. creates a diagonal square of 0s and 1s; see exercise 262.
;;;
;;; Finally, use loops to define `tabulate` from exercise 250.

; up-til: Number -> [List-of Number]
; Generates a list of natural numbers in the range [0, n)
(check-expect (up-til 0) '())
(check-expect (up-til 5) '(0 1 2 3 4))
(define (up-til n) (for/list [(i n)] i))

; iota: Number -> [List-of Number]
; Generates a list of natural numbers in the range [1, n]
(check-expect (iota 0) '())
(check-expect (iota 5) '(1 2 3 4 5))
(define (iota n) (for/list [(i n)] (add1 i)))

; inverses: Number -> [List-of Number]
; Generates a list that follows the sequence { 1, 1/2, 1/3 ... 1/n }
(check-expect (inverses 0) '())
(check-expect (inverses 5) '(1 1/2 1/3 1/4 1/5))
(define (inverses n) (for/list [(i (iota n))] (/ 1 i)))

; n-evens: Number -> [List-of Number]
; Generates a list of the first `n` even numbers
(check-expect (n-evens 0) '())
(check-expect (n-evens 5) '(2 4 6 8 10))
(define (n-evens n) (for/list [(i (iota n))] (* 2 i)))

; identityM: Number -> [List-of [List-of Number]]
; Generates an identity matrix of size `n` x `n`
(check-expect (identityM 1) '((1)))
(check-expect (identityM 3) '((1 0 0) (0 1 0) (0 0 1)))
(define (identityM n)
  (for/list [(i (iota n))]
    (for/list [(j (iota n))]
      (if (= i j) 1 0))))

; tabulate: Number [Number -> Number] -> [List-of Number]
; Generates a list whose elements are { f(0), f(1) ... f(n) }
(check-expect (tabulate 5 identity) '(0 1 2 3 4 5))
(check-within (tabulate 3 sqrt) (list (sqrt 0) (sqrt 1) (sqrt 2) (sqrt 3)) 0.0001)
(define (tabulate n f) (for/list [(i (add1 n))] (f i)))
