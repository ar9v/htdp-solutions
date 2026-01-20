#lang htdp/isl+

;;; Design a data representation for finite and infinite sets so that you can represent
;;; the sets of all odd numbers, all even numbers, all numbers divisible by 10, and so on.

; A Set is a function:
;   [X -> Boolean]
;
; interpretation: Set `(s x)` determines whether an element `x` belongs to set `s`

;;; Design the functions `add-element`, which adds an element to a set; `union`, which
;;; combines the elements of two sets; and `intersect`, which collects all elements common
;;; to two sets
;;;
;;; HINT:
;;; Mathematicians deal with sets as functions that consume a potential element `ed` and
;;; produce `#true` only if `ed` belongs to the set.

(define evens (λ (n) (= (remainder n 2) 0)))
(define multiples-of-5 (λ (n) (= (remainder n 5) 0)))

; add-element: X Set -> Set
; Add element `x` to set `s`
(check-expect ((add-element 3 evens) 3) #true)
(define (add-element x s)
  (λ (n) (or (s n) (equal? n x))))

; union: Set Set -> Set
; Given sets `s1` and `s2`, create a new Set that accepts all elements of s1 and s2
(check-expect ((union evens multiples-of-5) 10505) #true)
(check-expect ((union evens multiples-of-5) 10502) #true)
(check-expect ((union evens multiples-of-5) 10503) #false)
(define (union s1 s2)
  (λ (n) (or (s1 n) (s2 n))))

; intersect: Set Set -> Set
; Given sets `s1` and `s2`, create a new Set that only accepts elements that are both in
; `s1` and `s2`
(check-expect ((intersect evens multiples-of-5) 10505) #false)
(check-expect ((intersect evens multiples-of-5) 10) #true)
(define (intersect s1 s2)
  (λ (n) (and (s1 n) (s2 n))))
