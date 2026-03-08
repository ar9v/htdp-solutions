#lang htdp/isl+

;;; Define `is-var?`, `is-λ?`, and `is-app?`, that is, predicates that distinguish
;;; variables from λ expressions and applications.

(define ex1 '(λ (x) x))
(define ex2 '(((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w)))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '(λ (x) z))

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


;;; Also define

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


;;; Design `declareds`, which produces the list of all symbols used as λ parameters in a
;;; λ term. Don't worry about duplicate symbols.

; declareds: Lam -> [List-of Symbol]
; Returns all the symbols in `lam` that appear as parameters within it.
;
; Assumptions: doesn't care about duplicates
(check-expect (declareds ex1) '(x))
(check-expect (declareds ex2) '(y x z w))
(check-expect (declareds ex3) '(y x))
(check-expect (declareds ex4) '(x))
(define (declareds lam)
  (cond [(is-var? lam) '()]
        [(is-λ? lam) (cons (λ-para lam) (declareds (λ-body lam)))]
        [(is-app? lam) (append (declareds (app-fun lam))
                               (declareds (app-arg lam)))]))
