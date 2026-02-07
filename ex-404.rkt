#lang htdp/isl+

;;; Design the function `andmap2`. It consumes a function `f` from two values to Boolean
;;; and two equally long lists. Its result is also a Boolean. Specifically, it applies
;;; `f` to pairs of corresponding values from the two lists, and if `f` always produces
;;; #true, `andmap2` produces #true, too. Otherwise, `andmap2` produces #false. In short,
;;; `andmap2` is like `andmap2` but for two lists.

; andmap2: [X X -> Boolean] [List-of X] [List-of X] -> Boolean
; #true if `f` produces true for all pairs of corresponding values from `l1` and `l2`
;
; Assumption: `l1` and `l2` are the same size
(check-expect (andmap2 < '() '()) #true)
(check-expect (andmap2 < '(1 2) '(3 4)) #true)
(check-expect (andmap2 < '(3 4) '(1 2)) #false)
(check-expect (andmap2 < '(1 4 1) '(2 3 2)) #false)
(check-error (andmap2 < '(1) '()))
(check-error (andmap2 < '() '(1)))
(define (andmap2 f l1 l2)
  (cond [(and (empty? l1) (empty? l2)) #true]
        [(and (cons? l1) (cons? l2))
         (and (f (first l1) (first l2)) (andmap2 f (rest l1) (rest l2)))]
        [else (error "andmap2: Lists must be the same size!")]))
