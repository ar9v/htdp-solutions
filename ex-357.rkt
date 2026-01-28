#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `eval-definition1`. The function consumes four arguments:
;;;
;;; 1. A BSL-fun-expr `ex`;
;;; 2. A symbol `f`, which represents a function name;
;;; 3. A symbol `x`, which represents the function's parameter; and
;;; 4. A BSL-fun-expr `b`, which represents the function's body.
;;;
;;; It determines the value of `ex`. When `eval-definition1` encounters an application of
;;; `f` to some argument, it
;;;
;;; 1. Evaluates the argument
;;; 2. Substitutes the value of the argument for `x` in `b`; and
;;; 3. Finally evaluates the resulting expression with `eval-definition1`
;;;
;;; Here's how to express the steps as code, assuming `arg` is the argument of the function
;;; application:
;;;
;;; (local ((define value (eval-definition1 arg f x b))
;;;         (define plugd (subst b x argv-value)))
;;;   (eval-definition1 plugd f x b))
;;;
;;; Notice that this line uses a form of recursion that has not been covered. The proper
;;; design of such functions is discussed in part V.
;;;
;;; If `eval-definition1` encounters a variable, it signals the same error as
;;; `eval-variable` from exercise 354. It also signals an error for function applications
;;; that refer to a function name other than `f`.

(define-struct add [left right])
(define-struct mul [left right])
(define-struct funcall [name arg])
; a BSL-fun-expr is one of
; -- Number
; -- Symbol
; -- (make-add BSL-fun-expr BSL-fun-expr)
; -- (make-mul BSL-fun-expr BSL-fun-expr)
; -- (make-funcall Symbol BSL-fun-expr)
(define bfexpr1 (make-funcall 'k (make-add 1 1)))
(define bfexpr2 (make-mul 5 bfexpr1))
(define bfexpr3 (make-mul (make-funcall 'i 5) bfexpr1))
(define bfexpr4 (make-add (make-funcall 'k 5) (make-funcall 'k (make-funcall 'k 7))))

(define double (make-mul 2 'x))
(define square (make-mul 'x 'x))
(define triple (make-mul 3 'y))

; e.g. `(eval-definition1 (make-funcall 'k 1) 'k 'x loop)`
(define loop (make-funcall 'k 3))

; eval-definition1: BSL-fun-expr Symbol Symbol BSL-fun-expr -> [Maybe BSL-value]
; Evaluates `ex` like previous definitions, in addition to applying `f` if it finds it
; in `ex`, by evaluating its argument and using the value of this evaluation as the
; thing to substitute for `x` in `b`
(check-expect (eval-definition1 3 'k 'x double) 3)
(check-error (eval-definition1 'y 'i 'z double))
(check-expect (eval-definition1 bfexpr1 'k 'x double) 4)
(check-expect (eval-definition1 bfexpr2 'k 'x square) 20)
(check-expect (eval-definition1 bfexpr4 'k 'y triple) 78)
(check-error (eval-definition1 bfexpr3 'k 'x double))
(check-error (eval-definition1 (make-add 'x 3) 'y double))
(check-error (eval-definition1 (make-add 'x 3) 'x square))
(define (eval-definition1 ex f x b)
  (match ex
    [(? number?) ex]
    [(? symbol?) (error "Error: " ex " is unbound")]
    [(add l r) (+ (eval-definition1 l f x b) (eval-definition1 r f x b))]
    [(mul l r) (* (eval-definition1 l f x b) (eval-definition1 r f x b))]
    [(funcall n a)
     (if (equal? f n)
         (local [(define arg-value (eval-definition1 a f x b))
                 (define plugd (subst b x arg-value))]
           (eval-definition1 plugd f x b))
         (error "Error: " n " is unbound!"))]))

; subst: BSL-fun-expr Symbol Number -> BSL-var-expr
; Replaces all occurrences of `x` in `bexpr` with `v`
(define (subst bexpr s v)
  (match bexpr
    [(? number?) bexpr]
    [(? symbol?) (if (equal? bexpr s) v bexpr)]
    [(add l r) (make-add (subst l s v) (subst r s v))]
    [(mul l r) (make-mul (subst l s v) (subst r s v))]
    [(funcall n arg) (make-funcall n (subst arg s v))]))
