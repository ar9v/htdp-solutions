#lang htdp/isl+

;;; Develop `is-index?`, a specification for `index`:

; index: X [List-of X] -> [Maybe N]
; determine the index of the first occurrence of `x` in `l`, `#false` otherwise.
(define (index x l)
  (cond [(empty? l) #false]
        [(cons? l) (if (equal? (first l) x)
                       0
                       (local [(define i (index x (rest l)))]
                         (if (boolean? i) i (+ i 1))))]))

; is-index?: X [List-of X] -> [[Maybe N] -> Boolean]
; Is:
; - `maybe-index` #false and `x` not a member of `l`? or
; - `maybe-index` an integer, and `x` a member of `l` which is found after traversing
;   `maybe-index` elements of `l`?
(define (is-index? x l)
  (λ (maybe-index)
    (or (and (false? maybe-index) (not (member? x l)))
        (and (member? x l)
             (is-ith-and-first? x maybe-index l)))))

; is-ith-and-first?: X N [List-of X] -> Boolean
; Is `x` the `i`th element of `l`?
(check-expect (is-ith-and-first? 'a 3 '()) #false)
(check-expect (is-ith-and-first? 'a 3 '(a b)) #false)
(check-expect (is-ith-and-first? 'a 3 '(a b c d e)) #false)
(check-expect (is-ith-and-first? 'a 3 '(b c d a e)) #true)
(check-expect (is-ith-and-first? 'a 3 '(a b c a e)) #false)
(define (is-ith-and-first? x i l)
  (cond [(empty? l) #false]
        [(zero? i) (equal? (first l) x)]
        [else (and (not (equal? (first l) x))
                   (is-ith-and-first? x (sub1 i) (rest l)))]))

;;; Use `is-index?` to formulate a `check-satisfied` test for `index`.
(define a-list (build-list 5000 (λ (n) (random 5000))))
(define a-num (random 5000))

(check-satisfied (index a-num a-list)
                 (is-index? a-num a-list))
