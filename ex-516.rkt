#lang htdp/isl+

;;; Redesign the `undeclareds function for the structure-based data representation from
;;; exercise 513.

; A Lam is one of
; -- Var
; -- LExpr
; -- App
;
; Var is a Symbol which is not 'λ

(define-struct lexpr [param body])
; An LExpr is a structure
;   (make-lexpr Symbol Lam)
;
; interpretation: (make-lexpr p b) represents a lambda expression with a param `p` and
; body `b`

(define-struct app [fun arg])
; An App (for Application) is a structure
;   (make-app Lam Lam)
;
; interpretation: (make-app f a) represents a function application where `f` is the
; function expression and `a` is the arg expression.

(define-struct declared [sym])
; A Declared is a structure
;  (make-declared Symbol)
;
; interpretation: (make-declared 'x) denotes that variable 'x is not free

(define-struct undeclared [sym])
; An Undeclared is a structure
;  (make-undeclared Symbol)
;
; interpretation: (make-undeclared 'x) denotes that variable 'x is free

(define ex1 (make-lexpr 'x 'x))
(define ex2 (make-lexpr 'x 'y))
(define ex3 (make-lexpr 'y (make-lexpr 'x 'y)))
(define ex4 (make-app (make-lexpr 'x (make-app 'x 'x))
                      (make-lexpr 'x (make-app 'x 'x))))

; Lam -> Lam
(check-expect (undeclareds ex1)
              (make-lexpr 'x (make-declared 'x)))
(check-expect (undeclareds ex2)
              (make-lexpr 'x (make-undeclared 'y)))
(check-expect (undeclareds ex3)
              (make-lexpr 'y (make-lexpr 'x (make-declared 'y))))
(check-expect (undeclareds ex4)
              (make-app (make-lexpr 'x (make-app (make-declared 'x) (make-declared 'x)))
                        (make-lexpr 'x (make-app (make-declared 'x) (make-declared 'x)))))
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond [(is-var? le)
                   (if (member? le declareds) (make-declared le) (make-undeclared le))]
                  [(lexpr? le)
                   (local ((define para (lexpr-param le))
                           (define body (lexpr-body le))
                           (define newd (cons para declareds)))
                     (make-lexpr para (undeclareds/a body newd)))]
                  [(app? le)
                   (local ((define fun (app-fun le))
                           (define arg (app-arg le)))
                     (make-app (undeclareds/a fun declareds)
                               (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; is-var?: Any -> Boolean
; Is `e` a variable?
(check-expect (is-var? 'a) #true)
(check-expect (is-var? '(λ (x) y)) #false)
(check-expect (is-var? '((λ (x) (x x)) (λ (x) (x x)))) #false)
(define (is-var? e)
  (and (symbol? e) (not (equal? e 'λ))))
