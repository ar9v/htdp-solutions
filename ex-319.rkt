#lang htdp/isl+

(require 2htdp/abstraction)

;;; Design `substitute`. It consumes an S-expression `s` and two symbols, `old` and `new`.
;;; The result is like `s` with all occurrences of `old` replaced by `new`

; atom?: S-exp -> Boolean
(define (atom? s-exp)
  (or (number? s-exp) (string? s-exp) (symbol? s-exp)))

; substitute: S-exp Symbol Symbol -> S-exp
; Produces a new S-exp where all occurrences of `old` in `s` have been replaced by
; `new`
(check-expect (substitute 'hello 'hello 'goodbye) 'goodbye)
(check-expect (substitute 'hey 'hello 'goodbye) 'hey)
(check-expect (substitute '() 'hello 'goodbye) '())
(check-expect (substitute '(hello) 'hello 'goodbye) '(goodbye))
(check-expect (substitute '(hello hello) 'hello 'goodbye) '(goodbye goodbye))
(check-expect (substitute '(hello (hey (hello (hi there (hello)) hey) hello))
                          'hello
                          'goodbye)
              '(goodbye (hey (goodbye (hi there (goodbye)) hey) goodbye)))
(define (substitute s old new)
  (local [(define (substitute-sl sl)
            (match sl
              ['() '()]
              [(cons sexp rst) (cons (substitute sexp old new)
                                     (substitute-sl rst))]))]
    (cond [(atom? s) (if (equal? s old) new s)]
          [else (substitute-sl s)])))
