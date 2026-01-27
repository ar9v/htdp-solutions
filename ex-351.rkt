#lang htdp/isl+

;;; Design `interpreter-expr`. The function accepts S-expressions. If `parse` recognizes
;;; them as BSL-expr, it produces their value. Otherwise, it signals the same error as
;;; `parse`.

(define WRONG "ERROR: could not parse the provided S-expr")

(define-struct add [left right])
(define-struct mul [left right])
; a BSL-expr is one of
; -- Number
; -- (make-add BSL-expr BSL-expr)
; -- (make-mul BSL-expr BSL-expr)
(define ex-1 (make-add 1 1))
(define ex-2 (make-mul 3 10))
(define ex-3 (make-add (make-mul 1 1) 10))

; interpreter-expr: S-expr -> BSL-value
; Computes the BSL value represented by `sexpr`, if it's a legal BSL-expr.
(check-error (interpreter-expr #false))
(check-error (interpreter-expr '(1 2 3 4 5)))
(check-error (interpreter-expr '(- 3 4)))
(check-expect (interpreter-expr '(+ (+ 1 2) (+ (* 3 4) (+ 5 6)))) 26)
(define (interpreter-expr sexpr)
  (eval-expression (parse sexpr)))

(check-expect (eval-expression 3) 3)
(check-expect (eval-expression ex-1) 2)
(check-expect (eval-expression ex-2) 30)
(check-expect (eval-expression ex-3) 11)
(define (eval-expression bexpr)
  (cond [(number? bexpr) bexpr]
        [(add? bexpr) (+ (eval-expression (add-left bexpr))
                         (eval-expression (add-right bexpr)))]
        [(mul? bexpr) (* (eval-expression (mul-left bexpr))
                         (eval-expression (mul-right bexpr)))]))

(check-expect (atom? 1) #true)
(check-expect (atom? "hey") #true)
(check-expect (atom? 'hey) #true)
(check-expect (atom? #true) #false)
(define (atom? sexpr)
  (or (number? sexpr) (string? sexpr) (symbol? sexpr)))

; S-expr -> BSL-expr
(check-expect (parse 3) 3)
(check-error (parse 'hey))
(check-error (parse "hello"))
(check-expect (parse '(+ (+ 1 2) (+ (* 3 4) (+ 5 6))))
              (make-add (make-add 1 2)
                        (make-add (make-mul 3 4)
                                  (make-add 5 6))))
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-expr
(check-expect (parse-sl '(+ 1 1)) (make-add 1 1))
(check-expect (parse-sl '(* 2 4)) (make-mul 2 4))
(check-expect (parse-sl '(* (+ 1 1) (* 2 4)))
              (make-mul (make-add 1 1) (make-mul 2 4)))
(check-error (parse-sl '(1)))
(check-error (parse-sl '(1 2 3)))
(check-error (parse-sl '(1 2 3 4 5)))
(check-error (parse-sl '(- 3 4)))
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error WRONG)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))

; Atom -> BSL-expr
(check-expect (parse-atom 3) 3)
(check-error (parse-atom "hey"))
(check-error (parse-atom 'hey))
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))
