#lang htdp/isl+

;;; Make up an ISL+ expression in which `x` occurs both free and bound. Formulate it as
;;; an element of Lam. Does `undeclareds` work properly on your expression?

(define bound-unbound-1
  '((λ (x) (x x)) x))
(define bound-unbound-2
  '(λ (y) (x (λ (x) (y x)))))

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))

; Lam -> Lam
(check-expect (undeclareds ex1) ex1)
(check-expect (undeclareds ex2) '(λ (x) *undeclared))
(check-expect (undeclareds ex3) ex3)
(check-expect (undeclareds ex4) ex4)
(check-expect (undeclareds bound-unbound-1) '((λ (x) (x x)) *undeclared))
(check-expect (undeclareds bound-unbound-2) '(λ (y) (*undeclared (λ (x) (y x)))))
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) le '*undeclared)]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (undeclareds/a fun declareds)
                       (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))

; is-var?: Any -> Boolean
; Is `e` a variable?
(check-expect (is-var? 'a) #true)
(check-expect (is-var? '(λ (x) y)) #false)
(check-expect (is-var? '((λ (x) (x x)) (λ (x) (x x)))) #false)
(define (is-var? e)
  (and (symbol? e) (not (equal? e 'λ))))

; is-λ?: Any -> Boolean
; Is `e` a lambda expression?
(check-expect (is-λ? 'a) #false)
(check-expect (is-λ? '(λ (x) x)) #true)
(check-expect (is-λ? '((λ (x) (x x)) (λ (x) (x x)))) #false)
(define (is-λ? e)
  (and (list? e)
       (= (length e) 3)
       (local [(define f (first e))
               (define s (second e))
               (define t (third e))]
         (and (equal? f 'λ)
              (and (list? s) (is-var? (first s)))
              (is-lam? t)))))

; is-app?: Any -> Boolean
; Is `e` an application?
(check-expect (is-app? 'a) #false)
(check-expect (is-app? '(λ (x) x)) #false)
(check-expect (is-app? '((λ (x) (x x)) (λ (x) (x x)))) #true)
(check-expect (is-app? '((λ (x) x) 'foo)) #true)
(define (is-app? e)
  (and (list? e)
       (local [(define f (first e))
               (define s (second e))]
         (and (is-lam? f) (is-lam? s)))))

; is-lam?: Any -> Boolean
; Is `e` a Lam?
(define (is-lam? e)
  (or (is-var? e) (is-λ? e) (is-app? e)))

; λ-para: λ-expr -> Symbol
; Given a lambda expression `lexpr`, returns its parameter.
(check-expect (λ-para '(λ (z) z)) 'z)
(define (λ-para lexpr)
  (first (second lexpr)))

; λ-body: λ-expr -> Lam
; Given a lambda expression `lexpr`, returns its body
(check-expect (λ-body '(λ (z) (z z))) '(z z))
(check-expect (λ-body '(λ (x) (λ (z) (x z)))) '(λ (z) (x z)))
(define (λ-body lexpr)
  (third lexpr))

; app-fun: App -> Lam
; Given an application `app`, returns the function part
(check-expect (app-fun '(z y)) 'z)
(check-expect (app-fun '((λ (x) (x x)) (λ (y) (y y)))) '(λ (x) (x x)))
(define (app-fun app)
  (first app))

; app-arg: App -> Lam
; Given an application `app`, returns the arg part
(check-expect (app-arg '(z y)) 'y)
(check-expect (app-arg '((λ (x) (x x)) (λ (y) (y y)))) '(λ (y) (y y)))
(define (app-arg app)
  (second app))
