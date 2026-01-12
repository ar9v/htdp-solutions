#lang htdp/isl

;;; You can design `build-list` and `foldl` with the design recipes that you know, but
;;; they are not going to be like the ones that ISL provides. For example, the design
;;; of your own `foldl` function requires a use of the list `reverse` function:

; [X Y] [X Y -> Y] Y [List-of X] -> Y
; f*oldl works just like foldl
(check-expect (f*oldl cons '() '(a b c))
              (foldl cons '() '(a b c)))
(check-expect (f*oldl / 1 '(6 3 2))
              (foldl / 1 '(6 3 2)))
(define (f*oldl f e l)
  (foldr f e (reverse l)))

;;; Design `build-l*st`, which works just like `build-lst`.
;;;
;;; HINT:
;;; Recall the `add-at-end` function from exercise 193.

; build-l*st: N [N -> X] -> [List-of X]
; Builds a list of length `n` by applying `f` to the numbers in [0, n). Works like
; ISL's `build-list`
(check-expect (build-l*st 3 sqr) (build-list 3 sqr))
(check-expect (build-l*st 5 add1) (build-list 5 add1))
(define (build-l*st n f)
  (cond [(zero? n) '()]
        [else (append (build-l*st (sub1 n) f)
                      (list (f (sub1 n))))]))
