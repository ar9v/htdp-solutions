#lang htdp/isl+

;;; Design `static-distance`. The function replaces all occurrences of variables with a
;;; natural number that represents how far away the declaring λ is. [...] A value of
;;; 0 denotes the first 'λ on the path to the root, 1 the second one and so on.

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))
(define ex5 '(λ (x) (λ (y) (λ (z) (z (x y))))))
(define ex6 '((λ (x) ((λ (y) (y x)) x)) (λ (z) z)))

; Lam -> Lam
(check-expect (static-distance ex1)
              '(λ (x) 0))
(check-expect (static-distance ex2)
              '(λ (x) (*undeclared y)))
(check-expect (static-distance ex3)
              '(λ (y) (λ (x) 1)))
(check-expect (static-distance ex4)
              '((λ (x) (0 0))
                (λ (x) (0 0))))
(check-expect (static-distance ex5)
              '(λ (x) (λ (y) (λ (z) (0 (2 1))))))
(check-expect (static-distance ex6)
              '((λ (x) ((λ (y) (0 1)) 0)) (λ (z) 0)))
(define (static-distance le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ
          ; parameters on the path from le0 to le
          (define (static-distance/a le declareds)
            (cond
              [(is-var? le)
               (local [(define maybe-i (index-of declareds le))]
                 (if (not (false? maybe-i)) maybe-i `(*undeclared ,le)))]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (static-distance/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (static-distance/a fun declareds)
                       (static-distance/a arg declareds)))])))
    (static-distance/a le0 '())))

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

; index-of: [List-of X] X -> [Maybe N]
; Returns the index of the first occurrence of `e` in `l`
(check-expect (index-of '(a b c) 'b) 1)
(check-expect (index-of '(a b c) 'd) #false)
(define (index-of l e)
  (local [(define (index/a lp i)
            (cond [(empty? lp) #f]
                  [(equal? (first lp) e) i]
                  [else (index/a (rest lp) (add1 i))]))]
    (index/a l 0)))
