#lang htdp/isl+

;;; Task 1:
;;; Recall the signature for `foldl`:
;;;
;;;   foldl: [X Y -> Y] Y [List-of X] -> Y
;;;
;;; It is also the signature of `f*ldl`. Formulate the signature for `foldr/a` and its
;;; accumulator invariant.
;;;
;;; HINT: Assume that the difference between `l0` and `l` is `(list x1 x2 x3)`. What is
;;; `a`, then?

; version 4
; foldl: [X Y -> Y] Y [List-of X] -> Y
(define (f*ldl f i l0)
  (local [; foldr/a: Y [List-of X] -> Y
          ;
          ; accumulator i: represents the result of having applied `f` to the elements in
          ; `l0` that are not in `l`.
          (define (foldr/a a l)
            (cond
              [(empty? l) a]
              [else (foldr/a (f (first l) a) (rest l))]))]
    (foldr/a i l0)))

;;; Task 2:
;;; Design `build-l*st` using an accumulator-style approach. The function must satisfy the
;;; following tests:
;;;
;;;   (check-expect (build-l*st n f) (build-list n f))
;;;
;;; For any natural number `n` and function `f`

; pick-one: [List-of X] -> X
; pick a random element from the given list l
(define (pick-one l) (list-ref l (random (length l))))
(define double (λ (x) (* x 2)))
(define a-slope (λ (x) (+ (* 13 x) 27)))

; build-l*st: N [N -> X] -> X
; Returns a list of `n` elements, where the ith element is the result of applying
; `f` to `i`.
(define n (random 500))
(define fs (list double add1 sub1 sqr a-slope))
(define f (pick-one fs))
(check-expect (build-l*st n f) (build-list n f))
(define (build-l*st n0 f)
  (local [; bl/a: N [List-of X] -> [List-of X]
          ;
          ; accumulator res: represents the list built with numbers in the range
          ; [n0 - 1, n)
          (define (bl/a n res)
            (cond [(zero? n) (cons (f n) res)]
                  [else (bl/a (sub1 n) (cons (f n) res))]))]
    (bl/a (sub1 n0) '())))
