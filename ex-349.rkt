#lang htdp/isl+

;;; Create tests for `parse` until DrRacket tells you that every element in the definitions
;;; area is covered during the test run.

(define WRONG "ERROR: could not parse the provided S-expr")

(define-struct add [left right])
(define-struct mul [left right])
; a BSL-expr is one of
; -- Number
; -- (make-add BSL-expr BSL-expr)
; -- (make-mul BSL-expr BSL-expr)

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
