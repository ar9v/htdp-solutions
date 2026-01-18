#lang htdp/isl+

;;; Develop `found?`, a specification for the `find` function:

; find: X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of `l` that starts with `x`, #false otherwise
(define (find x l)
  (cond [(empty? l) #false]
        [else (if (equal? (first l) x) l (find x (rest l)))]))

; found?: X [List-of X] -> [[Maybe [List-of X]] -> Boolean]
(define (found? x l)
  (λ (maybe-list)
    (or (and (false? maybe-list) (not (member? x l)))
        (and (member? x l)
             (equal? (append (take-until (λ (e) (equal? e x)) l) maybe-list)
                     l)))))

; take-until: [X -> Boolean] [List-of X] -> [List-of X]
; Creates a list of elements in `l` up until the first element of `l` fulfills `pred`
(check-expect (take-until even? '()) '())
(check-expect (take-until even? '(1 3 5 4 7 9)) '(1 3 5))
(define (take-until pred l)
  (foldr (λ (e acc) (if (not (pred e)) (cons e acc) '())) '() l))

;;; Use `found?` to formulate a `check-satisfied` test for `find`
(define a-list (build-list 5000 (λ (x) (random 5000))))
(define a-number (random 5000))
(check-satisfied (find a-number a-list)
                 (found? a-number a-list))
