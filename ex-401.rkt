#lang htdp/isl+

;;; Design `sexp=?`, a function that determines whether two S-expressions are equal. For
;;; convenience, here is the data definition in condensed form

; An S-expr is one of
; -- Atom
; -- [List-of S-Expr]
;
; An Atom is one of
; -- Number
; -- String
; -- Symbol

(define (atom? s) (or (number? s) (string? s) (symbol? s)))

;;; Whenever you use `check-expect`, it uses a function like `sexp=?` to check whether two
;;; arbitrary values are equal. If not, the check fails and `check-expect` reports it as
;;; such.

; sexp=?: S-expr S-expr -> Boolean
; True if `s1` is equal to `s2` (structurally)
;
; yeah, yeah, we could use `equal?` that's not the point
(check-expect (sexp=? 1 1) #true)
(check-expect (sexp=? 'a 'a) #true)
(check-expect (sexp=? "foo" "foo") #true)
(check-expect (sexp=? '(1 a "foo") '(1 a "foo")) #true)
(check-expect (sexp=? '(1 (a ("foo")) (((2)))) '(1 (a ("foo")) (((2))))) #true)
(check-expect (sexp=? 1 'foo) #false)
(check-expect (sexp=? '(1 (b)) '(1 (c))) #false)
(define (sexp=? s1 s2)
  (local [(define (atom=? a1 a2)
            (or (and (number? a1) (number? a2) (= a1 a2))
                (and (string? a1) (string? a2) (string=? a1 a2))
                (and (symbol? a1) (symbol? a2) (equal? a1 a2))))
          (define (sl=? l1 l2)
            (or (and (empty? l1) (empty? l2))
                (and (cons? l1)
                     (cons? l2)
                     (sexp=? (first l1) (first l2))
                     (sl=? (rest l1) (rest l2)))))]
    (or (and (atom? s1) (atom? s2) (atom=? s1 s2))
        (and (cons? s1) (cons? s2) (sl=? s1 s2)))))
