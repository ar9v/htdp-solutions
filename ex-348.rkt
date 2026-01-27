#lang htdp/isl+

;;; Develop a data representation for Boolean BSL expressions constructed from #true,
;;; #false, `and`, `or`, and `not`. Then design `eval-bool-epression`, which consumes
;;; (representations of) Boolean BSL expressions and computes their values. What
;;; kind of values do these Boolean expressions yield?

(define-struct bsl-and [left right])
(define-struct bsl-or [left right])
(define-struct bsl-not [val])
; a BSL-Bool-expr is one of
; -- #true
; -- #false
; -- (make-bsl-and BSL-Bool-expr BSL-Bool-expr)
; -- (make-bsl-or BSL-Bool-expr BSL-Bool-expr)
; -- (make-bsl-not BSL-Bool-expr)

; eval-bool-expression: BSL-Bool-expr -> Boolean
; Computes the (BSL) Boolean value represented by bsl-bool-expr
(check-expect (eval-bool-expression #false) #false)
(check-expect (eval-bool-expression #true) #true)
(check-expect (eval-bool-expression (make-bsl-not #true)) #false)
(check-expect (eval-bool-expression (make-bsl-and #false #true)) #false)
(check-expect (eval-bool-expression (make-bsl-or #false #true)) #true)
(check-expect (eval-bool-expression (make-bsl-not (make-bsl-or #false #true))) #false)
(define (eval-bool-expression bsl-bool-expr)
  (cond [(boolean? bsl-bool-expr) bsl-bool-expr]
        [(bsl-and? bsl-bool-expr)
         (and (eval-bool-expression (bsl-and-left bsl-bool-expr))
              (eval-bool-expression (bsl-and-right bsl-bool-expr)))]
        [(bsl-or? bsl-bool-expr)
         (or (eval-bool-expression (bsl-or-left bsl-bool-expr))
             (eval-bool-expression (bsl-or-right bsl-bool-expr)))]
        [(bsl-not? bsl-bool-expr)
         (not (eval-bool-expression (bsl-not-val bsl-bool-expr)))]))
