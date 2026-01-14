#lang htdp/isl

;;; The fold functions are so powerful that you can define almost any list processing
;;; function with them. Use `fold` to define `map`

; map-with-fold: [X -> Y] [List-of X] -> [List-of Y]
; `map`, but implemented with `foldr`
(check-expect (map-with-fold sqr '(1 2 3)) (map sqr '(1 2 3)))
(check-expect (map-with-fold string-length '("foo" "bar" "quux"))
              (map string-length '("foo" "bar" "quux")))
(define (map-with-fold f l)
  (local ((define (combine x res) (cons (f x) res)))
    (foldr combine '() l)))
