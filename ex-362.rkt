#lang htdp/isl+

(require 2htdp/abstraction)

;;; It is cumbersome to enter the structure-based data representation of BSL expressions
;;; and a definitions area. As the end of chapter 21.1 demonstrates, it is much easier to
;;; quote expressions and (lists of) definitions.
;;;
;;; Design a function `interpreter`. It consumes an `S-expr` and an `Sl`. The former is
;;; supposed to represent an expression and the latter a list of definitions. The function
;;; parses both with the appropriate parsing functions and then uses `eval-all` from
;;; exercise 361 to evaluate the expression.

(define-struct add [left right])
(define-struct mul [left right])
(define-struct funcall [name arg])
; a BSL-fun-expr is one of
; -- Number
; -- Symbol
; -- (make-add BSL-fun-expr BSL-fun-expr)
; -- (make-mul BSL-fun-expr BSL-fun-expr)
; -- (make-funcall BSL-fun-expr BSL-fun-expr)
(define two-times-pi-sexp '(* 2 close-to-pi))
(define two-times-pi-bexpr (make-mul 2 'close-to-pi))

(define triple-double-circle-area-sexp '(+ (area-of-circle 2) (triple 5)))
(define triple-double-circle-area-bexpr
  (make-add (make-funcall 'area-of-circle 2)
            (make-funcall 'triple 5)))

(define undefined-funcall-sexp '(notfound 3))
(define undefined-funcall (make-funcall 'notfound 3))

(define-struct fundef [name param body])
; A BSL-fun-def is a structure
;   (make-fundef Symbol Symbol BSL-fun-expr)
(define volume-of-10-cylinder-sexp
  '(define (volume-of-10-cylinder r)
     (* 10 (area-of-circle r))))
(define volume-of-10-cylinder
  (make-fundef 'volume-of-10-cylinder
               'r
               (make-mul 10 (make-funcall 'area-of-circle 'r))))

(define area-of-circle-sexp
  '(define (area-of-circle r)
     (* close-to-pi (* r r))))
(define area-of-circle
  (make-fundef 'area-of-circle
               'r
               (make-mul 'close-to-pi (make-mul 'r 'r))))

(define triple-sexp
  '(define (triple x) (* 3 x)))
(define triple (make-fundef 'triple 'x (make-mul 3 'x)))

; A BSL-da-all is a [AL Symbol [BSL-fun-def | BSL-fun-expr]]
; An [AL X Y] is a [List-of [Assocaition X Y]]
; An [Association X Y] is a two element list [List X Y]
(define ex-da
  (list
   (list 'volume-of-10-cylinder volume-of-10-cylinder)
   (list 'area-of-circle area-of-circle)
   (list 'triple triple)
   (list 'close-to-pi 3.14)))

(define ex-sl
  (list volume-of-10-cylinder-sexp
        area-of-circle-sexp
        triple-sexp
        '(define close-to-pi 3.14)))

; interpreter: S-expr Sl -> BSL-value
; Evaluates `sexpr` using `sl` as its definitions area, if possible
(check-expect (interpreter 1 ex-sl) 1)
(check-expect (interpreter 'close-to-pi ex-sl) 3.14)
(check-expect (interpreter two-times-pi-sexp ex-sl) (* 2 3.14))
(check-expect (interpreter triple-double-circle-area-sexp ex-sl)
              (+ (* 3.14 (* 2 2)) (* 3 5)))
(check-error (interpreter '(+ (+ notfound close-to-pi) 3) ex-sl))
(check-error (interpeter '(notfunction 3) ex-sl))
(check-error (interpreter '(+ volume-of-cylinder 2) ex-sl))
(check-error (interpreter '(close-to-pi 1) ex-sl))
(define (interpreter sexp sl)
  (local [(define bfexpr (parse sexp))
          (define da (parse-defs sl))]
    (eval-all bfexpr da)))

; parse: S-exp -> BSL-fun-expr
; Turns the S-expr representation of a BSL-fun-expr into a BSL-fun-expr.
(check-expect (parse 1) 1)
(check-expect (parse 'x) 'x)
(check-expect (parse two-times-pi-sexp) two-times-pi-bexpr)
(check-expect (parse triple-double-circle-area-sexp) triple-double-circle-area-bexpr)
(check-expect (parse undefined-funcall-sexp) undefined-funcall)
(check-error (parse "hey"))
(check-error (parse '(1 2 3)))
(check-error (parse '(foo 1 2)))
(check-error (parse '(define (foo bar) (+ 1 2))))
(check-error (parse '(define my-var 3)))
(define (parse s)
  (cond [(atom? s) (parse-atom s)]
        [else (parse-sl s)]))

; parse-atom: Atom -> BSL-expr
(check-expect (parse-atom 3) 3)
(check-expect (parse-atom 'x) 'x)
(check-error (parse-atom "hey"))
(check-error (parse-atom #true))
(define (parse-atom s)
  (cond [(or (number? s) (symbol? s)) s]
        [else (error "Error: cannot parse " s)]))

; parse-sl: SL -> BSL-expr
(check-expect (parse-sl two-times-pi-sexp) two-times-pi-bexpr)
(check-expect (parse-sl triple-double-circle-area-sexp) triple-double-circle-area-bexpr)
(check-expect (parse-sl undefined-funcall-sexp) undefined-funcall)
(check-expect (parse-sl '(k 4)) (make-funcall 'k 4))
(check-error (parse-sl '(1 2 3)))
(check-error (parse-sl '(1 2 3 4 5)))
(check-error (parse-sl '(- 3 4)))
(define (parse-sl s)
  (match s
    [(list '+ l r) (make-add (parse l) (parse r))]
    [(list '* l r) (make-mul (parse l) (parse r))]
    [(list (? symbol?) expr) (make-funcall (first s) (parse expr))]
    [other (error "Error: " other " does not represent a BSL-fun-expr.")]))

; parse-defs: Sl -> BSL-da-all
; Takes the S-expr representation of a definitions area and turns it into a
; BSL-da-all, if possible
(check-expect (parse-defs ex-sl) ex-da)
(check-error (parse-defs '(("foo" 3))))
(check-error (parse-defs '(3 4)))
(check-error (parse-defs '((define (f x y) (+ x y)))))
(check-error (parse-defs '((define ("f" x) (+ x x)))))
(check-error (parse-defs '((define x (* 2 4)))))
(define (parse-defs sl)
  (local [(define (parse-def def)
            (match def
              [(list 'define (? symbol?) (? number?)) (list (second def) (third def))]
              [(list 'define (list fname param) body)
               (if (and (symbol? fname) (symbol? param))
                   (list fname (make-fundef fname param (parse body)))
                   (error "Error: expected symbols for the name and param, got " def))]
              [other (error "Error: expected a valid definition, got " other)]))]
    (match sl
      ['() '()]
      [(cons assc rst) (cons (parse-def assc) (parse-defs rst))])))

; atom?: S-expr -> Boolean
(define (atom? sexpr)
  (or (number? sexpr) (string? sexpr) (symbol? sexpr)))

; eval-all: BSL-fun-expr BSL-da-all -> [Maybe BSL-value]
; Evaluates `bfexpr` in the context of its definitions area, `da`, if it is possible.
(define (eval-all bfexpr da)
  (match bfexpr
    [(? number?) bfexpr]
    [(? symbol?) (lookup-con-def da bfexpr)]
    [(add l r) (+ (eval-all l da) (eval-all r da))]
    [(mul l r) (* (eval-all l da) (eval-all r da))]
    [(funcall f arg)
     (local [(define arg-value (eval-all arg da))
             (define fun-def (lookup-fun-def da f))
             (define plugd
               (match fun-def [(fundef _n param body) (subst body param arg-value)]))]
       (eval-all plugd da))]))

; lookup-con-def: BSL-da-all Symbol -> [Maybe Number]
; looks up `s`'s value in `da`; signals an error if there's no value for it
(define (lookup-con-def da s)
  (match (assq s da)
    [(list s val)
     (if (number? val)
         val
         (error "Error: reference to function " s ", but not as a function call"))]
    [_else (error "Error: " s " is not defined")]))

; lookup-fun-def: BSL-da-all Symbol -> [Maybe BSL-fun-def]
; Looks up the definition of `f` in `da`; signals an error if it can't find it
(define (lookup-fun-def da f)
  (match (assq f da)
    [(list f val)
     (if (number? val)
         (error "Error: " f " is a constant, but expected a function")
         val)]
    [_else (error "Error: " f " is not defined")]))

; subst: BSL-fun-expr Symbol Number -> BSL-var-expr
; Replaces all occurrences of `x` in `bexpr` with `v`
(define (subst bexpr s v)
  (match bexpr
    [(? number?) bexpr]
    [(? symbol?) (if (equal? bexpr s) v bexpr)]
    [(add l r) (make-add (subst l s v) (subst r s v))]
    [(mul l r) (make-mul (subst l s v) (subst r s v))]
    [(funcall n arg) (make-funcall n (subst arg s v))]))
