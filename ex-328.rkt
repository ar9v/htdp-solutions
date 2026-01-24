#lang htdp/isl+

;;; Copy and paste figure 120 into DrRacket; include your test suite. Validate the test
;;; suite. As you read along the remainder of this section, perform the edits and rerun
;;; the test suites to confirm the validity of our arguments


;;; Define the `atom?` function
(define (atom? s-exp)
  (or (number? s-exp) (string? s-exp) (symbol? s-exp)))

; substitute: S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
(check-expect (substitute 'bye 'bye 'hi) 'hi)
(check-expect (substitute 'bye 'hey 'hi) 'bye)
(check-expect (substitute '(((world) bye) bye) 'world '42)
              '(((42) bye) bye))
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
(check-expect (substitute '(((world) bye) bye) 'hello '42)
              '(((world) bye) bye))
(define (substitute sexp old new)
  (local (; S-expr -> S-expr
          (define (for-sexp sexp)
            (cond
              [(atom? sexp) (for-atom sexp)]
              [else (for-sl sexp)]))
          ; SL -> S-expr
          (define (for-sl sl) (map for-sexp sl))
          ; Atom -> S-expr
          (define (for-atom at) (if (equal? at old) new at)))
    (for-sexp sexp)))
