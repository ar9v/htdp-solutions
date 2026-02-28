#lang htdp/isl+

;;; Develop a formula that describes the abstract running time of `relative->absolute`.
;;;
;;; HINT: Evaluate the expression
;;;
;;; (relative->absolute (build-list size add1))
;;;
;;; by hand. Start by replacing `size` with 1, 2, and 3. How many recursions of
;;; `relative->absolute` and `add-to-each` are required each time?
;;;
;;; NOTE: not considering `local`

; relative->absolute: [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
;
;   1 empty? (base case)
; + (1 empty? + 1 cons + 2 first + 1 rest) * n + (Sum(0, n-1): 5n + 1) == 1/2 * n(5n - 3)
; ---------------------------------------------------------------------------------------
;   5n + n/2(5n - 3) + 1
;
; If we expand this expression, we'll see that this algorithm is effectively O(n^2)
;
; E.g. (relative->absolute '(1 2 3)) == 15 + 18 + 1 == 34
;
; NOTE: I'm hiding the implicit `empty?`s and `rest`s
;
; (cons                -> empty?, cons       = 2
;  (first '(1 2 3))    -> first              = 1
;  (add-to-each        -> add                = 11
;   (first '(1 2 3))   -> first              = 1
;   (cons              -> empty?, rest, cons = 3
;    (first '(2 3))    -> first              = 1
;    (add-to-each      -> add                = 6
;     (first '(2 3))   -> first              = 1
;     (cons            -> empty?, rest, cons = 3
;      (first '(3))    -> first              = 1
;      (add-to-each    -> add                = 1
;       (first '(3))   -> first              = 1
;       '()))))))      -> empty?, rest       = 2

(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))
(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (add-to-each (first l) rest-of-l)))
            (cons (first l) adjusted))]))

; add-to-each: Number [List-of Number] -> [List-of Number]
; adds n to each number on l
;
;   1 empty? (base case)
; + (1 empty? + 1 cons + 1 first + rest) * n
; --------------------------------------
;  5n + 1
;
; E.g. (add-to-each 1 '(1 2 3))        -> 5n + 1 = 5(3) + 1 = 16
; (cons (+ (first '(1 2 3)) 1)         -> cons, +, first, rest, empty?
;       (cons (+ (first '(2 3)) 1)     -> cons, +, first, rest, empty?
;             (cons (+ (first '(3)) 1) -> cons, +, first, rest, empty?
;                   '()))) -> empty?
(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))
(define (add-to-each n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) n) (add-to-each n (rest l)))]))
